;;; wsp-db.el --- Read-only access to wa-archive's store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/wsp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; wa-archive keeps every WhatsApp message in an SQLite store.  This
;; library opens it read-only and runs every query wsp needs; no other
;; wsp library runs SQL.  Writes go through wa-archive's API
;; (`wsp-api'), never through this connection.
;;
;; The store's schema is wa-archive's (archive.go).  `wsp-db-open'
;; checks that every table and column read here exists and signals an
;; error naming what is missing, rather than showing partial chats.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Options

(defgroup wsp nil
  "WhatsApp client for Emacs on wa-archive."
  :group 'comm
  :prefix "wsp-")

(defun wsp-db--xdg (variable default)
  "Return the directory in environment VARIABLE, or DEFAULT under home."
  (let ((value (getenv variable)))
    (if (and value (not (string-empty-p value)))
        value
      (expand-file-name default "~"))))

(defcustom wsp-data-directory
  (expand-file-name "wa-archive" (wsp-db--xdg "XDG_DATA_HOME" ".local/share"))
  "Directory of wa-archive's data: the store, media and the API socket."
  :type 'directory
  :group 'wsp)

(defcustom wsp-state-directory
  (expand-file-name "wa-archive" (wsp-db--xdg "XDG_STATE_HOME" ".local/state"))
  "Directory of wa-archive's state: heartbeat and status markers."
  :type 'directory
  :group 'wsp)

(defun wsp-db-file ()
  "Return the path of wa-archive's message store."
  (expand-file-name "messages.db" wsp-data-directory))

;;;; Connection

(defvar wsp-db--connection nil
  "The read-only connection to the store, or nil.")

(defvar wsp-db--connection-file nil
  "The file `wsp-db--connection' was opened on.")

(define-error 'wsp-db-error "wsp store error")

(defconst wsp-db--required-columns
  '(("chats" "key" "name" "is_group" "updated_at")
    ("aliases" "alias" "key")
    ("messages" "chat_key" "id" "sender_key" "sender_name" "from_me" "ts"
     "text" "quoted_id" "media_type" "file_name" "source")
    ("mutations" "chat_key" "target_id" "kind" "ts" "text")
    ("reactions" "chat_key" "target_id" "sender_key" "from_me" "emoji" "ts")
    ("receipts" "chat_key" "message_id" "reader_key" "kind" "ts")
    ("chat_reads" "chat_key" "watermark_ts")
    ("read_messages" "chat_key" "id"))
  "Tables and columns wsp reads, as (TABLE COLUMN...).")

(defun wsp-db ()
  "Return the open read-only store connection, opening it if needed."
  (let ((file (wsp-db-file)))
    (unless (and wsp-db--connection
                 (equal file wsp-db--connection-file)
                 (sqlitep wsp-db--connection))
      (wsp-db-open file))
    wsp-db--connection))

(defun wsp-db-open (file)
  "Open FILE read-only as the store and check its schema."
  (wsp-db-close)
  (unless (sqlite-available-p)
    (signal 'wsp-db-error '("This Emacs has no SQLite support")))
  (unless (file-exists-p file)
    (signal 'wsp-db-error (list (format "wa-archive's store not found at %s" file))))
  (let ((db (sqlite-open file t)))
    (unless (sqlitep db)
      (signal 'wsp-db-error (list (format "Cannot open %s" file))))
    (condition-case err
        (wsp-db--check-schema db)
      (error (sqlite-close db)
             (signal (car err) (cdr err))))
    (setq wsp-db--connection db
          wsp-db--connection-file file)))

(defun wsp-db-close ()
  "Close the store connection, if open."
  (when (and wsp-db--connection (sqlitep wsp-db--connection))
    (ignore-errors (sqlite-close wsp-db--connection)))
  (setq wsp-db--connection nil
        wsp-db--connection-file nil))

(defun wsp-db--check-schema (db)
  "Signal `wsp-db-error' unless DB has every table and column wsp reads."
  (let (missing)
    (pcase-dolist (`(,table . ,columns) wsp-db--required-columns)
      (let ((have (mapcar #'cadr (sqlite-select db (format "PRAGMA table_info(%s)" table)))))
        (if (null have)
            (push table missing)
          (dolist (column columns)
            (unless (member column have)
              (push (concat table "." column) missing))))))
    (when missing
      (signal 'wsp-db-error
              (list (format "wa-archive's store lacks %s; is wa-archive up to date?"
                            (string-join (nreverse missing) ", ")))))))

(defun wsp-db-select (sql &optional params)
  "Run SQL with PARAMS on the store and return its rows."
  (sqlite-select (wsp-db) sql params))

(defun wsp-db-data-version ()
  "Return the store's data version, which changes when another connection commits."
  (caar (wsp-db-select "PRAGMA data_version")))

;;;; Chats

(defconst wsp-db--message-columns
  "m.id, m.sender_key, m.sender_name, m.from_me, m.ts, m.text, m.quoted_id,
   m.media_type, m.file_name, m.source, m.chat_key,
   (SELECT u.text FROM mutations u WHERE u.chat_key = m.chat_key
      AND u.target_id = m.id AND u.kind = 'edit' ORDER BY u.ts DESC LIMIT 1),
   EXISTS (SELECT 1 FROM mutations u WHERE u.chat_key = m.chat_key
      AND u.target_id = m.id AND u.kind = 'revoke')"
  "The columns of a message row, in the order `wsp-db--message' reads.")

(defun wsp-db--message (row)
  "Return message ROW as a plist."
  (pcase-let ((`(,id ,sender-key ,sender-name ,from-me ,ts ,text ,quoted-id
                     ,media-type ,file-name ,source ,chat ,edited ,revoked)
               row))
    (list :id id :chat chat :sender-key sender-key :sender-name sender-name
          :from-me (eql from-me 1) :ts ts
          :text (or edited text) :edited (and edited t)
          :revoked (eql revoked 1) :quoted-id quoted-id
          :media-type media-type :file-name file-name :source source)))

(defconst wsp-db--unread-condition
  "m.from_me = 0
   AND (m.text <> '' OR m.media_type IN
        ('', 'image', 'video', 'voice', 'audio', 'document', 'sticker'))
   AND m.ts > COALESCE((SELECT r.watermark_ts FROM chat_reads r
                        WHERE r.chat_key = m.chat_key), 0)
   AND NOT EXISTS (SELECT 1 FROM read_messages x
                   WHERE x.chat_key = m.chat_key AND x.id = m.id)
   AND NOT EXISTS (SELECT 1 FROM mutations u WHERE u.chat_key = m.chat_key
                   AND u.target_id = m.id AND u.kind = 'revoke')"
  "SQL condition true of an unread incoming message M.")

(defun wsp-db-chats ()
  "Return every chat that has messages, most recent first.
Each is a plist with :key, :name, :is-group, :last (the newest message)
and :unread (the number of unread incoming messages)."
  (let ((unread (make-hash-table :test #'equal)))
    (pcase-dolist (`(,key ,n) (wsp-db-select
                               (concat "SELECT m.chat_key, COUNT(*) FROM messages m WHERE "
                                       wsp-db--unread-condition " GROUP BY m.chat_key")))
      (puthash key n unread))
    (mapcar
     (lambda (row)
       (let ((last (wsp-db--message (nthcdr 3 row))))
         (list :key (nth 0 row) :name (nth 1 row) :is-group (eql (nth 2 row) 1)
               :last last :unread (gethash (nth 0 row) unread 0))))
     (wsp-db-select
      (concat "SELECT c.key, c.name, c.is_group, " wsp-db--message-columns "
               FROM messages m
               JOIN (SELECT chat_key, MAX(ts) AS mts FROM messages GROUP BY chat_key) l
                 ON l.chat_key = m.chat_key AND l.mts = m.ts
               JOIN chats c ON c.key = m.chat_key
               WHERE m.rowid = (SELECT m2.rowid FROM messages m2
                                WHERE m2.chat_key = m.chat_key AND m2.ts = l.mts
                                ORDER BY m2.rowid DESC LIMIT 1)
               ORDER BY m.ts DESC, m.rowid DESC")))))

(defun wsp-db-all-chats ()
  "Return every known chat, with or without messages, most recent first.
Each is a plist with :key, :name and :is-group."
  (mapcar (lambda (row)
            (list :key (nth 0 row) :name (nth 1 row) :is-group (eql (nth 2 row) 1)))
          (wsp-db-select "SELECT key, name, is_group FROM chats ORDER BY updated_at DESC")))

(defun wsp-db-chat (key)
  "Return chat KEY as a plist with :key, :name and :is-group, or nil."
  (when-let* ((row (car (wsp-db-select
                         "SELECT key, name, is_group FROM chats WHERE key = ?"
                         (list key)))))
    (list :key (nth 0 row) :name (nth 1 row) :is-group (eql (nth 2 row) 1))))

(defun wsp-db-canonical (key)
  "Return the canonical chat key for KEY, which may be an alias."
  (or (caar (wsp-db-select "SELECT key FROM aliases WHERE alias = ?" (list key)))
      key))

(defun wsp-db-aliases (key)
  "Return the aliases that map to canonical chat KEY."
  (mapcar #'car (wsp-db-select "SELECT alias FROM aliases WHERE key = ?" (list key))))

(defun wsp-db-latest-push-name (key)
  "Return the latest push name used by the other party of direct chat KEY."
  (caar (wsp-db-select
         "SELECT sender_name FROM messages WHERE chat_key = ? AND from_me = 0
          AND sender_name <> '' ORDER BY ts DESC LIMIT 1"
         (list key))))

(defun wsp-db-jid-number (jid)
  "Return the phone number in JID with a leading +, or JID itself."
  (if (string-match "\\`\\([0-9]+\\)\\(?::[0-9]+\\)?@s\\.whatsapp\\.net\\'" jid)
      (concat "+" (match-string 1 jid))
    jid))

(defun wsp-db-display-name (chat)
  "Return the name to show for CHAT, a plist from `wsp-db-chat'.
An unnamed direct chat is named after its contact's latest push name,
else the number; an unnamed group after its key."
  (let ((name (plist-get chat :name))
        (key (plist-get chat :key)))
    (cond ((and name (not (string-empty-p name))) name)
          ((plist-get chat :is-group) (format "Group %s" key))
          ((let ((push (wsp-db-latest-push-name key)))
             (and push (not (string-empty-p push)) push)))
          (t (wsp-db-jid-number key)))))

(defvar wsp-db--names nil
  "Cache of chat names by key for sender names, or nil.
Valid while `wsp-db--names-version' equals the store's data version.")

(defvar wsp-db--push-names nil
  "Cache of `wsp-db--sender-push-name' results, valid with `wsp-db--names'.")

(defvar wsp-db--names-version nil
  "The data version `wsp-db--names' was built at.")

(defun wsp-db-contact-name (key)
  "Return the name of the direct chat with KEY, or nil if it has none."
  (let ((version (wsp-db-data-version)))
    (unless (and wsp-db--names (equal version wsp-db--names-version))
      (setq wsp-db--names (make-hash-table :test #'equal)
            wsp-db--push-names (make-hash-table :test #'equal)
            wsp-db--names-version version)
      (pcase-dolist (`(,k ,name) (wsp-db-select
                                  "SELECT key, name FROM chats WHERE name <> ''"))
        (puthash k name wsp-db--names)))
    (gethash key wsp-db--names)))

(defun wsp-db-sender-name (msg)
  "Return the name to show for the sender of MSG, a message plist."
  (if (plist-get msg :from-me)
      "Me"
    (let ((key (plist-get msg :sender-key))
          (push (plist-get msg :sender-name)))
      (or (and key (wsp-db-contact-name key))
          (and push (not (string-empty-p push)) push)
          (and key (not (string-empty-p key))
               (or (wsp-db--sender-push-name key) (wsp-db-jid-number key)))
          "?"))))

(defun wsp-db--sender-push-name (key)
  "Return the latest push name the sender KEY used in any chat, or nil.
The store has no index on senders, so results are cached until the
store changes."
  (wsp-db-contact-name key)
  (let ((cached (gethash key wsp-db--push-names 'none)))
    (if (not (eq cached 'none))
        cached
      (let* ((name (caar (wsp-db-select
                          "SELECT sender_name FROM messages WHERE sender_key = ?
                           AND sender_name <> '' ORDER BY ts DESC LIMIT 1"
                          (list key))))
             (value (and name (not (string-empty-p name)) name)))
        (puthash key value wsp-db--push-names)
        value))))

(defun wsp-db-unread-total ()
  "Return the number of unread incoming messages across all chats."
  (caar (wsp-db-select
         (concat "SELECT COUNT(*) FROM messages m WHERE " wsp-db--unread-condition))))

(defun wsp-db-unread-count (key)
  "Return the number of unread incoming messages in chat KEY."
  (caar (wsp-db-select
         (concat "SELECT COUNT(*) FROM messages m WHERE m.chat_key = ? AND "
                 wsp-db--unread-condition)
         (list key))))

;;;; Messages

(defun wsp-db-messages (key since)
  "Return the messages of chat KEY with a time at or after SINCE, oldest first.
SINCE is a Unix time in seconds; nil returns every message."
  (mapcar #'wsp-db--message
          (wsp-db-select
           (concat "SELECT " wsp-db--message-columns " FROM messages m
                    WHERE m.chat_key = ? AND m.ts >= ? ORDER BY m.ts, m.rowid")
           (list key (or since 0)))))

(defun wsp-db-page-boundary (key before count)
  "Return the time of the COUNTth newest message of chat KEY before BEFORE.
BEFORE is a Unix time, or nil for no bound.  When fewer messages
remain, return the time of the oldest; return nil when none remain."
  (let ((bound (or before most-positive-fixnum)))
    (or (caar (wsp-db-select
               "SELECT ts FROM messages WHERE chat_key = ? AND ts < ?
                ORDER BY ts DESC LIMIT 1 OFFSET ?"
               (list key bound (max 0 (1- count)))))
        (caar (wsp-db-select
               "SELECT MIN(ts) FROM messages WHERE chat_key = ? AND ts < ?"
               (list key bound))))))

(defun wsp-db-message (key id)
  "Return message ID of chat KEY as a plist, or nil."
  (when-let* ((row (car (wsp-db-select
                         (concat "SELECT " wsp-db--message-columns
                                 " FROM messages m WHERE m.chat_key = ? AND m.id = ?")
                         (list key id)))))
    (wsp-db--message row)))

(defun wsp-db-chats-with-message (id)
  "Return the keys of the chats that hold a message with ID."
  (mapcar #'car (wsp-db-select "SELECT DISTINCT chat_key FROM messages WHERE id = ?"
                               (list id))))

(defun wsp-db-reactions (key ids)
  "Return the current reactions in chat KEY to the messages IDS.
The value is a hash table from message id to a list of plists with
:emoji, :sender-key and :from-me."
  (let ((table (make-hash-table :test #'equal)))
    (when ids
      (pcase-dolist (`(,target ,emoji ,sender ,from-me)
                     (wsp-db-select
                      (format "SELECT target_id, emoji, sender_key, from_me FROM reactions
                               WHERE chat_key = ? AND emoji <> '' AND target_id IN (%s)
                               ORDER BY ts"
                              (wsp-db--placeholders ids))
                      (cons key ids)))
        (push (list :emoji emoji :sender-key sender :from-me (eql from-me 1))
              (gethash target table))))
    (maphash (lambda (k v) (puthash k (nreverse v) table)) table)
    table))

(defun wsp-db-receipts (key ids)
  "Return the receipt state of your messages IDS in chat KEY.
The value is a hash table from message id to `read' or `delivered'."
  (let ((table (make-hash-table :test #'equal)))
    (when ids
      (pcase-dolist (`(,id ,kind)
                     (wsp-db-select
                      (format "SELECT message_id, kind FROM receipts
                               WHERE chat_key = ? AND message_id IN (%s)"
                              (wsp-db--placeholders ids))
                      (cons key ids)))
        (if (member kind '("read" "played"))
            (puthash id 'read table)
          (unless (gethash id table)
            (puthash id 'delivered table)))))
    table))

(defun wsp-db--placeholders (items)
  "Return a comma-separated list of one SQL placeholder per element of ITEMS."
  (mapconcat #'identity (make-list (length items) "?") ","))

(defun wsp-db-search (query &optional key limit)
  "Return messages whose text, latest edit or file name contains QUERY.
With KEY, search only that chat.  Return at most LIMIT (default 200),
newest first."
  (let ((like (concat "%" (wsp-db--escape-like query) "%")))
    (mapcar #'wsp-db--message
            (wsp-db-select
             (concat "SELECT " wsp-db--message-columns " FROM messages m
                      WHERE (m.text LIKE ?1 ESCAPE '\\' OR m.file_name LIKE ?1 ESCAPE '\\'
                             OR EXISTS (SELECT 1 FROM mutations u
                                        WHERE u.chat_key = m.chat_key AND u.target_id = m.id
                                        AND u.kind = 'edit' AND u.text LIKE ?1 ESCAPE '\\'))
                      AND NOT EXISTS (SELECT 1 FROM mutations u WHERE u.chat_key = m.chat_key
                                      AND u.target_id = m.id AND u.kind = 'revoke')"
                     (if key " AND m.chat_key = ?2" " AND ?2 IS NULL")
                     " ORDER BY m.ts DESC LIMIT ?3")
             (list like key (or limit 200))))))

(defun wsp-db--escape-like (string)
  "Escape the LIKE wildcards in STRING with backslashes."
  (replace-regexp-in-string "[%_\\\\]" "\\\\\\&" string))

(defun wsp-db-new-incoming (after-rowid)
  "Return live incoming messages stored after AFTER-ROWID, and the newest rowid.
The value is (ROWID . MESSAGES); ROWID is AFTER-ROWID when nothing is
new.  Only recent messages that arrived live count: history replays
add rows marked as history, and an alias merge copies old rows under
new rowids, so neither notifies anybody."
  (let ((max (or (caar (wsp-db-select "SELECT MAX(rowid) FROM messages")) 0)))
    (cons max
          (when (> max after-rowid)
            (mapcar #'wsp-db--message
                    (wsp-db-select
                     (concat "SELECT " wsp-db--message-columns " FROM messages m
                              WHERE m.rowid > ? AND m.from_me = 0 AND m.source = 'live'
                              AND m.ts >= ? ORDER BY m.rowid")
                     (list after-rowid (- (floor (float-time)) 600))))))))

;;;; Health

(defcustom wsp-heartbeat-max-age 3600
  "Seconds after which wa-archive's heartbeat counts as stale."
  :type 'natnum
  :group 'wsp)

(defun wsp-db-health ()
  "Return a warning about wa-archive's state, or nil when it looks healthy.
This reads the files the daemon keeps in `wsp-state-directory': the
markers it leaves when unlinked or rejected as outdated, and its
heartbeat."
  (let ((unlinked (expand-file-name "unlinked" wsp-state-directory))
        (outdated (expand-file-name "outdated" wsp-state-directory))
        (heartbeat (expand-file-name "heartbeat.json" wsp-state-directory)))
    (cond
     ((file-exists-p outdated)
      "WhatsApp rejected wa-archive as outdated; no messages are arriving")
     ((file-exists-p unlinked)
      "wa-archive is unlinked from WhatsApp; no messages are arriving")
     ((not (file-exists-p heartbeat))
      "wa-archive has no heartbeat; it may not be running")
     (t
      (let* ((beat (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents heartbeat)
                       (json-parse-buffer :object-type 'plist))))
             (at (and beat (plist-get beat :at)
                      (ignore-errors (float-time (date-to-time (plist-get beat :at)))))))
        (cond ((null at) "wa-archive's heartbeat is unreadable")
              ((> (- (float-time) at) wsp-heartbeat-max-age)
               (format "wa-archive's heartbeat is %d minutes old; recent messages may be missing"
                       (floor (- (float-time) at) 60)))))))))

(defun wsp-db-health-signature ()
  "Return a value that changes when the files `wsp-db-health' reads change."
  (mapcar (lambda (name)
            (let ((file (expand-file-name name wsp-state-directory)))
              (and (file-exists-p file)
                   (file-attribute-modification-time (file-attributes file)))))
          '("unlinked" "outdated" "heartbeat.json")))

(provide 'wsp-db)
;;; wsp-db.el ends here
