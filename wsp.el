;;; wsp.el --- WhatsApp client on wa-archive  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/wsp
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: comm

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

;; wsp is a WhatsApp client for Emacs.  It has no WhatsApp connection of
;; its own: wa-archive, a daemon linked to the phone as a companion
;; device, holds the connection and every message.  wsp reads
;; wa-archive's store and acts through its local API, as sgn does with
;; signal-cli.
;;
;; How the parts fit together:
;;
;; - `wsp-db' reads the store, read-only.
;; - `wsp-api' talks to the daemon over a Unix socket: actions out,
;;   events in.
;; - `wsp-list' is the chat list, `wsp-chat' a chat, `wsp-actions' the
;;   commands on a message, `wsp-media' attachments, `wsp-search'
;;   search and `wsp-notify' notifications and the unread indicator.
;; - This file holds the entry points, opening a chat at a message, and
;;   the refresh that redraws open buffers when the store,
;;   the media or the daemon's health changes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wsp-db)
(require 'wsp-api)
(require 'wsp-media)
(require 'wsp-chat)
(require 'wsp-actions)
(require 'wsp-list)
(require 'wsp-notify)
(require 'wsp-search)

;;;; Options

(defcustom wsp-refresh-interval 3
  "Seconds between checks for changes the daemon did not announce.
New messages arrive at once through the API; this check catches
downloads, the daemon's health, and changes made while disconnected."
  :type 'number
  :group 'wsp)

(defcustom wsp-send-presence nil
  "Whether to show you as online while you use wsp.
When non-nil, wsp tells WhatsApp you are available while Emacs has
focus and a wsp buffer is selected, and unavailable otherwise."
  :type 'boolean
  :group 'wsp)

(defvar wsp-health-warning nil
  "A warning about wa-archive's state, or nil when it looks healthy.")

;;;; Entry points

;;;###autoload
(defun wsp ()
  "Show the list of WhatsApp chats."
  (interactive)
  (wsp-start)
  (wsp-list-show))

;;;###autoload
(defun wsp-chat (chat)
  "Open the WhatsApp chat CHAT, a chat key.
Interactively, choose a chat by name; a number with a leading + opens
a chat with that number."
  (interactive (list (wsp--read-chat)))
  (wsp-start)
  (wsp-chat-open chat))

(defun wsp--read-chat ()
  "Read a chat by name and return its key."
  (let* ((chats (wsp-db-all-chats))
         (table (make-hash-table :test #'equal))
         candidates)
    (dolist (chat chats)
      (let* ((name (wsp-db-display-name chat))
             (label (if (gethash name table)
                        (format "%s (%s)" name (wsp-db-jid-number (plist-get chat :key)))
                      name)))
        (puthash label (plist-get chat :key) table)
        (push label candidates)))
    (let ((choice (completing-read "Chat: " (nreverse candidates))))
      (or (gethash choice table)
          (wsp--number-key choice)
          (user-error "No chat named %s" choice)))))

(defun wsp--number-key (string)
  "Return the chat key for phone number STRING, or nil if it is not one."
  (let ((digits (replace-regexp-in-string "[ ()-]" "" string)))
    (when (string-match "\\`\\+\\([0-9]\\{6,15\\}\\)\\'" digits)
      (concat (match-string 1 digits) "@s.whatsapp.net"))))

;;;; Opening at a message

(define-error 'wsp-message-not-found "WhatsApp message not found" 'user-error)

(defun wsp--parse-ref (ref)
  "Split REF, \"CHAT_KEY/MSG_ID\", into (CHAT-KEY . MSG-ID)."
  (let ((slash (and (stringp ref) (string-search "/" ref))))
    (unless (and slash (> slash 0) (< (1+ slash) (length ref)))
      (user-error "Not a wa-archive message reference: %S" ref))
    (let ((cut (cl-position ?/ ref :from-end t)))
      (cons (substring ref 0 cut) (substring ref (1+ cut))))))

(defun wsp-message-buffer (ref)
  "Return the chat buffer with point at the message REF, without showing it.
REF is wa-archive's message reference, \"CHAT_KEY/MSG_ID\", by which
other programs can record a message.  The chat key may be an alias the
chat had when the reference was recorded.

If the message is not in the store (it was never received, or the
reference is wrong), signal `wsp-message-not-found' with data
\(REASON CHAT-BUFFER), where CHAT-BUFFER is the chat's buffer when the
chat is known, else nil."
  (wsp-start)
  (pcase-let* ((`(,chat . ,id) (wsp--parse-ref ref))
               (canonical (wsp-db-canonical chat))
               (holder (if (wsp-db-message canonical id)
                           canonical
                         (let ((holders (wsp-db-chats-with-message id)))
                           (and (= (length holders) 1) (car holders))))))
    (cond
     (holder
      (let ((buffer (wsp-chat-buffer holder)))
        (with-current-buffer buffer
          (wsp-chat-show-message id))
        buffer))
     ((wsp-db-chat canonical)
      (signal 'wsp-message-not-found
              (list "That message is not in wa-archive's store" (wsp-chat-buffer canonical))))
     (t
      (signal 'wsp-message-not-found
              (list (format "No chat %s in wa-archive's store" chat) nil))))))

;;;###autoload
(defun wsp-open-message (ref)
  "Show the chat of the message REF with point on that message.
REF is wa-archive's message reference, \"CHAT_KEY/MSG_ID\".  If the
message is gone but its chat is known, show the chat and say why."
  (interactive (list (read-string "wa-archive message reference (CHAT/ID): ")))
  (condition-case err
      (let ((buffer (wsp-message-buffer ref)))
        (pop-to-buffer buffer)
        buffer)
    (wsp-message-not-found
     (pcase-let ((`(,reason ,buffer) (cdr err)))
       (if (buffer-live-p buffer)
           (progn (pop-to-buffer buffer)
                  (message "%s" reason)
                  buffer)
         (signal (car err) (cdr err)))))))

;;;; Session and refresh

(defvar wsp--timer nil
  "The refresh timer, or nil when wsp is not running.")

(defvar wsp--data-version nil
  "The store's data version at the last refresh.")

(defvar wsp--connected nil
  "Whether wsp was connected to wa-archive at the last tick.")

(defvar wsp--dirty nil
  "Chats to redraw at the next refresh: a list of keys, or t for all.")

(defvar wsp--refresh-scheduled nil
  "Non-nil while a refresh is scheduled.")

(defvar wsp--failures 0
  "Consecutive refresh failures.")

(defvar wsp--presence-state nil
  "The presence last sent, or nil.")

(defun wsp-start ()
  "Start wsp's connection to wa-archive and its refresh, if not running."
  (wsp-db)
  (unless wsp--timer
    (setq wsp--failures 0
          wsp--dirty t)
    (add-hook 'wsp-api-event-functions #'wsp--on-event)
    (add-hook 'wsp-api-status-functions #'wsp--on-status)
    (add-hook 'window-selection-change-functions #'wsp--on-selection)
    (add-function :after after-focus-change-function #'wsp--on-focus)
    (add-hook 'kill-emacs-hook #'wsp--on-exit)
    (setq wsp--timer (run-with-timer wsp-refresh-interval wsp-refresh-interval #'wsp--tick))
    (wsp-api-start)
    (wsp--refresh)))

(defun wsp-stop ()
  "Stop wsp's refresh and disconnect from wa-archive.
Chat buffers stay, but are no longer updated."
  (interactive)
  (wsp--send-presence "unavailable")
  (when wsp--timer (cancel-timer wsp--timer))
  (setq wsp--timer nil)
  (remove-hook 'wsp-api-event-functions #'wsp--on-event)
  (remove-hook 'wsp-api-status-functions #'wsp--on-status)
  (remove-hook 'window-selection-change-functions #'wsp--on-selection)
  (remove-function after-focus-change-function #'wsp--on-focus)
  (wsp-api-stop))

(defun wsp--on-exit ()
  "Save every chat's draft and stop wsp, as Emacs exits."
  (wsp-chat-save-all-drafts)
  (wsp-stop))

(defun wsp--live-buffers-p ()
  "Return non-nil if any wsp buffer exists."
  (or (wsp-list-buffer) (wsp-chat-buffers)))

(defun wsp--tick ()
  "Catch what the daemon did not announce; stop when no wsp buffer is left.
While connected, the daemon announces every change to the store, so a
store change found here only refreshes the list and notifications;
without a connection, every chat is redrawn.  A chat whose media
directories changed (a download, a transcript) is redrawn.  The
health warning is recomputed each time, since a heartbeat goes stale
without any file changing."
  (if (not (wsp--live-buffers-p))
      (wsp-stop)
    (condition-case err
        (let ((version (wsp-db-data-version))
              (connected (wsp-api-connected-p))
              (health (wsp-db-health)))
          (unless (equal health wsp-health-warning)
            (setq wsp-health-warning health)
            (force-mode-line-update t))
          (unless (eq connected wsp--connected)
            (setq wsp--connected connected)
            (force-mode-line-update t))
          (dolist (buffer (wsp-chat-buffers))
            (when (wsp-chat-media-changed-p buffer)
              (wsp--mark-dirty (list (buffer-local-value 'wsp-chat-key buffer)))))
          (unless (equal version wsp--data-version)
            (unless connected (setq wsp--dirty t))
            (setq wsp--data-version version)
            (wsp-refresh-soon))
          (when wsp--dirty (wsp-refresh-soon))
          (setq wsp--failures 0))
      (error
       (setq wsp--failures (1+ wsp--failures))
       (let ((inhibit-message (< wsp--failures 3)))
         (message "wsp: refresh failed: %s" (error-message-string err)))
       (when (>= wsp--failures 3)
         (wsp-db-close)
         (wsp-stop))))))

(defun wsp--mark-dirty (keys)
  "Add the chats KEYS, or t for all, to those the next refresh redraws."
  (unless (eq wsp--dirty t)
    (setq wsp--dirty (if (eq keys t) t (append keys wsp--dirty)))))

(defun wsp-refresh-soon ()
  "Refresh shortly, once however often this is called."
  (unless wsp--refresh-scheduled
    (setq wsp--refresh-scheduled t)
    (run-with-timer 0.1 nil (lambda ()
                              (setq wsp--refresh-scheduled nil)
                              (wsp--refresh)))))

(defun wsp--refresh ()
  "Redraw the list and the chats marked dirty, and update notifications.
A failure in one buffer is logged and does not stop the others."
  (let ((dirty wsp--dirty))
    (setq wsp--dirty nil
          wsp--data-version (ignore-errors (wsp-db-data-version))
          wsp-health-warning (ignore-errors (wsp-db-health)))
    (wsp--guarded "the chat list" #'wsp-list-refresh)
    (dolist (buffer (wsp-chat-buffers))
      (when (or (eq dirty t)
                (member (buffer-local-value 'wsp-chat-key buffer)
                        (mapcar #'wsp-db-canonical dirty)))
        (wsp--guarded (buffer-name buffer) #'wsp-chat-redraw buffer)))
    (wsp--guarded "notifications" #'wsp-notify-check)
    (when wsp-indicator-mode
      (wsp--guarded "the indicator" #'wsp-notify-update-indicator))
    (force-mode-line-update t)))

(defun wsp--guarded (what function &rest args)
  "Call FUNCTION with ARGS, logging an error about WHAT instead of signaling it."
  (condition-case err
      (apply function args)
    (error (let ((inhibit-message t))
             (message "wsp: could not refresh %s: %s" what (error-message-string err))))))

(defun wsp--on-event (event)
  "Handle EVENT from the daemon."
  (pcase (plist-get event :event)
    ("changed"
     (wsp--mark-dirty (or (plist-get event :chats) t))
     (wsp-chat-recheck-uncertain)
     (wsp-refresh-soon))
    ("status" (force-mode-line-update t))))

(defun wsp--on-status ()
  "Handle a change of the API connection.
Redraw everything, since changes may have been missed, ask about sends
whose outcome is unknown, and re-send presence."
  (setq wsp--presence-state nil)
  (wsp--update-presence)
  (when (wsp-api-connected-p)
    (wsp-chat-recheck-uncertain))
  (wsp--mark-dirty t)
  (wsp-refresh-soon))

;;;; Focus, read state and presence

(defun wsp--on-selection (&rest _)
  "Mark the selected chat read and update presence after a window change."
  (wsp-chat-mark-read-visible)
  (wsp--update-presence))

(defun wsp--on-focus ()
  "Mark the selected chat read and update presence after a focus change."
  (when (eq (frame-focus-state (selected-frame)) t)
    (wsp-chat-mark-read-visible))
  (wsp--update-presence))

(defun wsp--update-presence ()
  "Send the presence `wsp-send-presence' calls for, if it changed."
  (when wsp-send-presence
    (wsp--send-presence
     (if (and (eq (frame-focus-state (selected-frame)) t)
              (with-current-buffer (window-buffer (selected-window))
                (derived-mode-p 'wsp-chat-mode 'wsp-list-mode)))
         "available"
       "unavailable"))))

(defun wsp--send-presence (state)
  "Tell WhatsApp you are STATE, when `wsp-send-presence' is on and it changed."
  (when (and wsp-send-presence (wsp-api-connected-p)
             (not (equal state wsp--presence-state)))
    (setq wsp--presence-state state)
    (ignore-errors (wsp-api-request "presence" (list :state state) #'ignore))))

(provide 'wsp)
;;; wsp.el ends here
