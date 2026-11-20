;;; wsp-chat.el --- Chat buffers for wsp  -*- lexical-binding: t; -*-

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

;; A chat buffer shows one chat, drawn from wa-archive's store, with a
;; prompt and an input area below the history, as sgn's do.  Each chat
;; has at most one buffer, found by its canonical key.
;;
;; The buffer remembers the time of the oldest message it shows (its
;; boundary), not a count: every redraw shows everything from the
;; boundary to the newest message, so new arrivals never push loaded
;; history, or a message opened with `wsp-open-message', out of it.
;; A redraw keeps point and each window's start on the same message,
;; and keeps the input and its undo history.
;;
;; Sending goes through wa-archive's API.  A message appears in the
;; history once the daemon has stored it; until then it is shown below
;; the history as pending.  If the outcome is unknown (the answer was
;; lost), the text stays there marked "may have been sent" and is never
;; sent again on its own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wsp-db)
(require 'wsp-api)
(require 'wsp-media)

(declare-function wsp-refresh-soon "wsp")
(defvar wsp-health-warning)

;;;; Options and faces

(defcustom wsp-history-page-size 200
  "How many messages a chat buffer first shows.
Each \\[wsp-chat-load-more] adds as many more."
  :type 'natnum
  :group 'wsp)

(defcustom wsp-message-grouping-interval 300
  "Seconds within which a sender's messages share one header."
  :type 'natnum
  :group 'wsp)

(defcustom wsp-prompt "> "
  "The prompt before the input area of a chat buffer."
  :type 'string
  :group 'wsp)

(defcustom wsp-mark-read t
  "Whether showing a chat marks its messages read, sending read receipts."
  :type 'boolean
  :group 'wsp)

(defcustom wsp-send-typing nil
  "Whether to tell the chat when you are typing in its input area."
  :type 'boolean
  :group 'wsp)

(defcustom wsp-drafts-file (locate-user-emacs-file "wsp-drafts.eld")
  "File where unsent input is kept between sessions."
  :type 'file
  :group 'wsp)

(defface wsp-header-face '((t :inherit font-lock-keyword-face :weight bold))
  "Face of the header above a sender's messages."
  :group 'wsp)

(defface wsp-own-header-face '((t :inherit font-lock-function-name-face :weight bold))
  "Face of the header above your own messages."
  :group 'wsp)

(defface wsp-timestamp-face '((t :inherit shadow))
  "Face of timestamps."
  :group 'wsp)

(defface wsp-quote-face '((t :inherit font-lock-comment-face :slant italic))
  "Face of the quoted message a reply shows."
  :group 'wsp)

(defface wsp-deleted-face '((t :inherit shadow :slant italic))
  "Face of deleted messages."
  :group 'wsp)

(defface wsp-annotation-face '((t :inherit shadow))
  "Face of annotations such as (edited) and receipt marks."
  :group 'wsp)

(defface wsp-read-face '((t :inherit link :underline nil))
  "Face of the mark of a message the recipient read."
  :group 'wsp)

(defface wsp-media-face '((t :inherit font-lock-constant-face))
  "Face of attachment labels."
  :group 'wsp)

(defface wsp-system-face '((t :inherit shadow))
  "Face of WhatsApp records that are not messages."
  :group 'wsp)

(defface wsp-reaction-face '((t :inherit shadow))
  "Face of reactions."
  :group 'wsp)

(defface wsp-pending-face '((t :inherit shadow :slant italic))
  "Face of a message being sent."
  :group 'wsp)

(defface wsp-error-face '((t :inherit error))
  "Face of a message whose sending failed or may have failed."
  :group 'wsp)

(defface wsp-target-face '((t :inherit highlight :extend t))
  "Face of the message `wsp-open-message' opened at."
  :group 'wsp)

(defface wsp-prompt-face '((t :inherit minibuffer-prompt))
  "Face of the prompt."
  :group 'wsp)

;;;; State

(defvar-local wsp-chat-key nil
  "The canonical key of this buffer's chat.")

(defvar-local wsp-chat--since nil
  "Time of the oldest message the buffer shows, or nil before the first draw.")

(defvar-local wsp-chat--input-marker nil
  "Marker at the start of the input area.")

(defvar-local wsp-chat--pending nil
  "Sends without a final outcome, as plists with :op, :text and :state.
STATE is `sending' or `uncertain'.")

(defvar-local wsp-chat--reply-to nil
  "The message plist the next send replies to, or nil.")

(defvar-local wsp-chat--edit-target nil
  "The message plist the next send replaces, or nil.")

(defvar-local wsp-chat--signature nil
  "The media signature at the last redraw.")

(defvar-local wsp-chat--target-overlay nil
  "Overlay on the message `wsp-open-message' opened at, or nil.")

(defvar-local wsp-chat--marking-read nil
  "Non-nil while a mark-read request for this chat is in flight.")

(defvar-local wsp-chat--typing-sent nil
  "Time typing was last announced, or nil.")

(defvar-local wsp-chat--typing-timer nil
  "Timer that announces the end of typing, or nil.")

(defvar wsp-chat--buffers (make-hash-table :test #'equal)
  "Map from canonical chat key to its buffer.")

(defvar wsp-chat--fetch-requested (make-hash-table :test #'equal)
  "Attachments already asked for, as \"CHAT/ID\" keys.")

;;;; Buffers

(defun wsp-chat-buffers ()
  "Return the live chat buffers."
  (let (buffers)
    (maphash (lambda (_ buffer) (when (buffer-live-p buffer) (push buffer buffers)))
             wsp-chat--buffers)
    buffers))

(defun wsp-chat-buffer (key)
  "Return the chat buffer for chat KEY, creating and drawing it if needed.
KEY may be an alias; the buffer belongs to its canonical key."
  (let* ((key (wsp-db-canonical key))
         (buffer (gethash key wsp-chat--buffers)))
    (if (buffer-live-p buffer)
        buffer
      (setq buffer (generate-new-buffer (wsp-chat--buffer-name key)))
      (puthash key buffer wsp-chat--buffers)
      (with-current-buffer buffer
        (wsp-chat-mode)
        (setq wsp-chat-key key)
        (wsp-chat--redraw)
        (wsp-chat--restore-draft)
        (goto-char (point-max)))
      buffer)))

(defun wsp-chat--buffer-name (key)
  "Return the name for the buffer of chat KEY."
  (format "*wsp: %s*" (wsp-chat--name key)))

(defun wsp-chat--name (key)
  "Return the display name of chat KEY."
  (wsp-db-display-name (or (wsp-db-chat key) (list :key key))))

(defun wsp-chat-open (key)
  "Display the chat buffer for chat KEY and select it."
  (let ((buffer (wsp-chat-buffer key)))
    (pop-to-buffer buffer)
    buffer))

(defun wsp-chat--unregister ()
  "Forget this buffer and save its input as a draft.
Say so if sends of unknown outcome are being forgotten with it."
  (wsp-chat--save-draft)
  (when-let* ((uncertain (seq-filter (lambda (p) (eq (plist-get p :state) 'uncertain))
                                     wsp-chat--pending)))
    (message "wsp: %d message(s) in %s may or may not have been sent; check your phone"
             (length uncertain) (buffer-name)))
  (when (eq (gethash wsp-chat-key wsp-chat--buffers) (current-buffer))
    (remhash wsp-chat-key wsp-chat--buffers)))

;;;; Mode

(defvar wsp-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wsp-chat-return)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-l") #'wsp-chat-load-more)
    (define-key map (kbd "C-c C-a") #'wsp-chat-attach-file)
    (define-key map (kbd "C-c C-k") #'wsp-chat-cancel)
    (define-key map (kbd "C-c C-s") #'wsp-search-chat)
    map)
  "Keymap of chat buffers.")

(defvar wsp-chat-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'wsp-chat-reply)
    (define-key map (kbd "e") #'wsp-chat-edit)
    (define-key map (kbd "d") #'wsp-chat-delete)
    (define-key map (kbd "+") #'wsp-chat-react)
    (define-key map (kbd "w") #'wsp-chat-copy)
    (define-key map (kbd "o") #'wsp-chat-open-at-point)
    (define-key map (kbd "g") #'wsp-chat-load-more)
    (define-key map (kbd "n") #'wsp-chat-next-message)
    (define-key map (kbd "p") #'wsp-chat-previous-message)
    (define-key map (kbd "x") #'wsp-chat-dismiss-pending)
    map)
  "Keymap active on messages in the history.")

(declare-function wsp-search-chat "wsp-search")
(declare-function wsp-chat-reply "wsp-actions")
(declare-function wsp-chat-edit "wsp-actions")
(declare-function wsp-chat-delete "wsp-actions")
(declare-function wsp-chat-react "wsp-actions")
(declare-function wsp-chat-copy "wsp-actions")
(declare-function wsp-chat-open-at-point "wsp-actions")
(declare-function wsp-chat-attach-file "wsp-actions")
(declare-function wsp-chat-dismiss-pending "wsp-actions")

(define-derived-mode wsp-chat-mode fundamental-mode "wsp"
  "Major mode of a wsp chat buffer.

\\{wsp-chat-mode-map}
On a message:
\\{wsp-chat-message-map}"
  (setq-local buffer-undo-list t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local header-line-format '(:eval (wsp-chat--header-line)))
  (setq-local wsp-chat--input-marker (point-min-marker))
  (add-hook 'kill-buffer-hook #'wsp-chat--unregister nil t)
  (add-hook 'after-change-functions #'wsp-chat--after-change nil t)
  (setq buffer-undo-list nil))

(defun wsp-chat--header-line ()
  "Return the header line of the chat buffer."
  (let ((chat (wsp-db-chat wsp-chat-key)))
    (concat
     " " (propertize (wsp-chat--name wsp-chat-key) 'face 'bold)
     (when (plist-get chat :is-group) (propertize "  group" 'face 'shadow))
     (cond (wsp-chat--edit-target (propertize "  editing a message (C-c C-k cancels)" 'face 'warning))
           (wsp-chat--reply-to
            (propertize (format "  replying to %s: %s (C-c C-k cancels)"
                                (wsp-db-sender-name wsp-chat--reply-to)
                                (truncate-string-to-width
                                 (replace-regexp-in-string
                                  "\n" " " (or (plist-get wsp-chat--reply-to :text) "")) 30 nil nil "…"))
                        'face 'warning)))
     (unless (wsp-api-connected-p)
       (propertize "  not connected to wa-archive: read-only" 'face 'warning))
     (when (bound-and-true-p wsp-health-warning)
       (propertize (concat "  " wsp-health-warning) 'face 'error)))))

;;;; Drawing

(defun wsp-chat-redraw (buffer)
  "Redraw chat BUFFER from the store if it is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (wsp-chat--redraw))))

(defun wsp-chat-media-changed-p (buffer)
  "Return non-nil if the media directories of chat BUFFER changed."
  (with-current-buffer buffer
    (not (equal wsp-chat--signature (wsp-media-directories-signature wsp-chat-key)))))

(defvar wsp-chat--width nil
  "Width in columns the history is drawn for, bound while drawing.")

(defun wsp-chat--redraw ()
  "Redraw the current chat buffer, keeping input, point and window starts.
The history is drawn into a string first, so that a failed query or
\\[keyboard-quit] while drawing leaves the buffer, and above all the
input, as it was."
  (unless wsp-chat--since
    (setq wsp-chat--since (wsp-db-page-boundary wsp-chat-key nil wsp-history-page-size)))
  (let* ((history (wsp-chat--render))
         (inhibit-read-only t)
         (input (wsp-chat--input-text))
         (anchors (wsp-chat--save-positions))
         (point-anchor (wsp-chat--anchor (point)))
         (undo buffer-undo-list)
         (old-input-start (marker-position wsp-chat--input-marker)))
    (setq wsp-chat--signature (wsp-media-directories-signature wsp-chat-key))
    (let ((buffer-undo-list t)
          (inhibit-quit t))
      (erase-buffer)
      (insert history)
      (set-marker wsp-chat--input-marker (point))
      (insert input))
    (setq buffer-undo-list
          (wsp-chat--shift-undo undo (- wsp-chat--input-marker old-input-start)))
    (goto-char (wsp-chat--resolve-anchor point-anchor))
    (wsp-chat--restore-positions anchors)
    (rename-buffer (wsp-chat--buffer-name wsp-chat-key) t)
    (wsp-chat--schedule-mark-read)))

(defun wsp-chat--render ()
  "Return the history, pending sends and prompt of this chat as a string."
  (let ((key wsp-chat-key)
        (since wsp-chat--since)
        (pending wsp-chat--pending)
        (wsp-chat--width (window-body-width (or (get-buffer-window nil t) (selected-window))))
        (wsp-media--listing (make-hash-table :test #'equal))
        (inhibit-read-only t))
    (with-temp-buffer
      (setq-local wsp-chat-key key
                  wsp-chat--since since
                  wsp-chat--pending pending)
      (wsp-chat--insert-history (wsp-db-messages key since))
      (wsp-chat--insert-pending)
      (wsp-chat--insert-prompt)
      (buffer-string))))

(defun wsp-chat--input-text ()
  "Return the text of the input area."
  (if (and wsp-chat--input-marker (marker-position wsp-chat--input-marker))
      (buffer-substring-no-properties wsp-chat--input-marker (point-max))
    ""))

(defun wsp-chat--shift-undo (undo delta)
  "Return UNDO with its buffer positions moved by DELTA.
The input moves when the history above it changes length, so its undo
entries must move with it."
  (if (or (zerop delta) (eq undo t))
      undo
    (mapcar (lambda (entry) (wsp-chat--shift-undo-entry entry delta)) undo)))

(defun wsp-chat--shift-undo-entry (entry delta)
  "Return undo ENTRY with its positions moved by DELTA."
  (pcase entry
    ((pred integerp) (+ entry delta))
    (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
     (cons (+ beg delta) (+ end delta)))
    (`(,(and text (pred stringp)) . ,(and pos (pred integerp)))
     (cons text (if (< pos 0) (- pos delta) (+ pos delta))))
    (`(nil ,prop ,val ,beg . ,end) `(nil ,prop ,val ,(+ beg delta) . ,(+ end delta)))
    (`(apply . ,_) entry)
    (_ entry)))

(defun wsp-chat--anchor (pos)
  "Return a description of POS that survives a redraw."
  (cond
   ((>= pos wsp-chat--input-marker) (cons 'input (- pos wsp-chat--input-marker)))
   ((get-text-property pos 'wsp-id)
    (let ((start (or (and (> pos (point-min))
                          (equal (get-text-property (1- pos) 'wsp-id)
                                 (get-text-property pos 'wsp-id))
                          (previous-single-property-change pos 'wsp-id))
                     pos)))
      (list 'message (get-text-property pos 'wsp-id) (- pos start)
            (get-text-property pos 'wsp-ts))))
   (t (list 'near (wsp-chat--nearest-ts pos)))))

(defun wsp-chat--nearest-ts (pos)
  "Return the time of the first message at or after POS, or nil."
  (let ((next (if (get-text-property pos 'wsp-id)
                  pos
                (next-single-property-change pos 'wsp-id))))
    (and next (get-text-property next 'wsp-ts))))

(defun wsp-chat--resolve-anchor (anchor)
  "Return the position ANCHOR describes in the redrawn buffer."
  (pcase anchor
    (`(input . ,offset) (min (point-max) (+ wsp-chat--input-marker offset)))
    (`(message ,id ,offset ,ts)
     (if-let* ((start (wsp-chat--message-start id)))
         (min (+ start offset)
              (or (next-single-property-change start 'wsp-id) wsp-chat--input-marker))
       (wsp-chat--position-at-or-after ts)))
    (`(near ,ts) (wsp-chat--position-at-or-after ts))
    (_ (point-max))))

(defun wsp-chat--position-at-or-after (ts)
  "Return the start of the first message at or after time TS, or the input."
  (or (and ts
           (let ((pos (point-min)) found)
             (while (and (not found)
                         (setq pos (if (get-text-property pos 'wsp-id)
                                       pos
                                     (next-single-property-change pos 'wsp-id))))
               (if (>= (get-text-property pos 'wsp-ts) ts)
                   (setq found pos)
                 (setq pos (next-single-property-change pos 'wsp-id))))
             found))
      (marker-position wsp-chat--input-marker)))

(defun wsp-chat--message-start (id)
  "Return the position where message ID starts, or nil."
  (let ((pos (point-min)) found)
    (while (and (not found) pos (< pos (point-max)))
      (if (equal (get-text-property pos 'wsp-id) id)
          (setq found pos)
        (setq pos (next-single-property-change pos 'wsp-id))))
    found))

(defun wsp-chat--save-positions ()
  "Return the point and start of every window showing this buffer."
  (mapcar (lambda (window)
            (list window
                  (wsp-chat--anchor (window-point window))
                  (wsp-chat--anchor (window-start window))))
          (get-buffer-window-list nil nil t)))

(defun wsp-chat--restore-positions (anchors)
  "Restore the window points and starts saved in ANCHORS."
  (pcase-dolist (`(,window ,point ,start) anchors)
    (when (window-live-p window)
      (set-window-point window (wsp-chat--resolve-anchor point))
      (set-window-start window (wsp-chat--resolve-anchor start) t))))

(defun wsp-chat--insert-history (messages)
  "Insert MESSAGES, grouped under headers by sender and time."
  (let* ((ids (mapcar (lambda (m) (plist-get m :id)) messages))
         (reactions (wsp-db-reactions wsp-chat-key ids))
         (receipts (wsp-db-receipts
                    wsp-chat-key
                    (mapcar (lambda (m) (plist-get m :id))
                            (seq-filter (lambda (m) (plist-get m :from-me)) messages))))
         (group (plist-get (wsp-db-chat wsp-chat-key) :is-group))
         last-sender last-ts)
    (when (and messages (wsp-db-page-boundary wsp-chat-key wsp-chat--since 1))
      (insert (propertize "  (older messages: C-c C-l)\n" 'face 'shadow
                          'read-only t 'front-sticky '(read-only))))
    (unless messages
      (insert (propertize "  No messages yet.\n" 'face 'shadow
                          'read-only t 'front-sticky '(read-only))))
    (dolist (msg messages)
      (let ((sender (if (plist-get msg :from-me) :me (plist-get msg :sender-key)))
            (ts (plist-get msg :ts))
            (start (point)))
        (unless (and (equal sender last-sender) last-ts
                     (< (- ts last-ts) wsp-message-grouping-interval)
                     (equal (wsp-chat--day ts) (wsp-chat--day last-ts)))
          (wsp-chat--insert-header msg group))
        (wsp-chat--insert-message msg reactions receipts)
        (add-text-properties start (point)
                             (list 'wsp-id (plist-get msg :id) 'wsp-ts ts
                                   'keymap wsp-chat-message-map
                                   'read-only t 'front-sticky '(read-only)))
        (setq last-sender sender last-ts ts)))))

(defun wsp-chat--day (ts)
  "Return the local date of Unix time TS as a string."
  (format-time-string "%F" ts))

(defun wsp-chat--insert-header (msg group)
  "Insert the header for MSG's sender and time.
In a GROUP chat the header names the sender; in a direct chat too,
since both sides alternate."
  (ignore group)
  (let* ((own (plist-get msg :from-me))
         (label (format "── %s · " (wsp-db-sender-name msg)))
         (time (wsp-chat-format-time (plist-get msg :ts)))
         (width (or wsp-chat--width 80))
         (fill (max 2 (- width (string-width label) (string-width time) 2))))
    (insert (propertize label 'face (if own 'wsp-own-header-face 'wsp-header-face))
            (propertize time 'face 'wsp-timestamp-face)
            " "
            (propertize (make-string fill ?─) 'face 'wsp-timestamp-face)
            "\n")))

(defun wsp-chat-format-time (ts)
  "Return Unix time TS formatted for a header."
  (let ((now (decode-time))
        (then (decode-time ts)))
    (cond ((equal (wsp-chat--day ts) (format-time-string "%F"))
           (format-time-string "%H:%M" ts))
          ((= (decoded-time-year now) (decoded-time-year then))
           (format-time-string "%a %e %b %H:%M" ts))
          (t (format-time-string "%F %H:%M" ts)))))

(defun wsp-chat--insert-message (msg reactions receipts)
  "Insert MSG with its quote, media, annotations and reactions.
REACTIONS and RECEIPTS are the tables of `wsp-db-reactions' and
`wsp-db-receipts'."
  (let ((id (plist-get msg :id)))
    (when (and (plist-get msg :quoted-id) (not (string-empty-p (plist-get msg :quoted-id))))
      (wsp-chat--insert-quote (plist-get msg :quoted-id)))
    (cond
     ((plist-get msg :revoked)
      (insert (propertize "  [deleted]" 'face 'wsp-deleted-face) "\n"))
     ((wsp-media-system-p msg)
      (insert (propertize (concat "  " (wsp-media-label msg)) 'face 'wsp-system-face) "\n"))
     (t (wsp-chat--insert-body msg (gethash id receipts))))
    (when-let* ((list (gethash id reactions)))
      (insert "  " (propertize (wsp-chat--reactions-text list) 'face 'wsp-reaction-face) "\n"))))

(defun wsp-chat--insert-body (msg receipt)
  "Insert MSG's media and text, then its annotations and RECEIPT mark."
  (let ((text (plist-get msg :text))
        (kind (plist-get msg :media-type)))
    (when (and kind (not (string-empty-p kind)))
      (wsp-chat--insert-media msg)
      (when (or (null text) (string-empty-p text))
        ;; The media line ends the message unless something follows on it.
        (if (or (plist-get msg :edited) (plist-get msg :from-me))
            (delete-char -1)
          (setq text nil))))
    (when (and text (not (string-empty-p text)))
      (insert "  " (wsp-chat--indent text)))
    (when (plist-get msg :edited)
      (insert (propertize " (edited)" 'face 'wsp-annotation-face)))
    (when (plist-get msg :from-me)
      (insert " " (pcase receipt
                    ('read (propertize "✓✓" 'face 'wsp-read-face))
                    ('delivered (propertize "✓✓" 'face 'wsp-annotation-face))
                    (_ (propertize "✓" 'face 'wsp-annotation-face)))))
    (unless (and kind (not (string-empty-p kind)) (null text))
      (insert "\n"))))

(defun wsp-chat--indent (text)
  "Return TEXT with its continuation lines indented like the first."
  (replace-regexp-in-string "\n" "\n  " text))

(defun wsp-chat--insert-media (msg)
  "Insert the attachment of MSG: an image inline, else its label."
  (let* ((id (plist-get msg :id))
         (file (wsp-media-file wsp-chat-key id)))
    (insert "  " (propertize (wsp-media-label msg) 'face 'wsp-media-face))
    (cond
     ((member (plist-get msg :media-type) '("voice" "audio"))
      (let ((transcript (wsp-media-transcript wsp-chat-key id)))
        (insert (propertize (if file " (RET plays)" " (RET fetches and plays)")
                            'face 'wsp-annotation-face))
        (when transcript
          (insert "\n  " (propertize (concat "“" transcript "”") 'face 'wsp-quote-face)))))
     ((and file (wsp-media-image-p msg) wsp-inline-images (display-images-p))
      (insert "\n")
      (wsp-media-insert-image file)
      (delete-char -1))
     (file (insert (propertize " (RET opens)" 'face 'wsp-annotation-face)))
     ((wsp-media-fetchable-p msg)
      (when (and (wsp-media-image-p msg) wsp-auto-fetch-images)
        (wsp-chat--fetch msg nil))
      (insert (propertize " (not downloaded; RET fetches)" 'face 'wsp-annotation-face))))
    (insert "\n")))

(defun wsp-chat--fetch (msg callback)
  "Ask wa-archive to download MSG's attachment, once per session.
CALLBACK, if non-nil, is called with the file's path when it arrives."
  (let ((key (concat wsp-chat-key "/" (plist-get msg :id))))
    (when (and (wsp-api-connected-p)
               (or callback (not (gethash key wsp-chat--fetch-requested))))
      (puthash key t wsp-chat--fetch-requested)
      (wsp-api-request
       "fetch" (list :chat wsp-chat-key :id (plist-get msg :id))
       (lambda (result error)
         (if error
             (when callback
               (message "wsp: download failed: %s" (plist-get error :message)))
           (when callback (funcall callback (plist-get result :path)))))
       :timeout 130))))

(defun wsp-chat--insert-quote (quoted-id)
  "Insert the quote of message QUOTED-ID."
  (let* ((quoted (wsp-db-message wsp-chat-key quoted-id))
         (text (cond ((null quoted) "[message not in archive]")
                     ((plist-get quoted :revoked) "[deleted]")
                     (t (let ((body (or (plist-get quoted :text) "")))
                          (if (string-empty-p body) (wsp-media-label quoted) body)))))
         (author (if quoted (concat (wsp-db-sender-name quoted) ": ") "")))
    (insert (propertize (concat "  ┃ " author
                                (truncate-string-to-width
                                 (replace-regexp-in-string "\n" " " text) 60 nil nil "…"))
                        'face 'wsp-quote-face)
            "\n")))

(defun wsp-chat--reactions-text (reactions)
  "Return REACTIONS grouped by emoji with counts."
  (let (counts)
    (dolist (reaction reactions)
      (let* ((emoji (plist-get reaction :emoji))
             (cell (assoc emoji counts)))
        (if cell (setcdr cell (1+ (cdr cell))) (push (cons emoji 1) counts))))
    (mapconcat (lambda (cell)
                 (if (= (cdr cell) 1) (car cell) (format "%s %d" (car cell) (cdr cell))))
               (nreverse counts) "  ")))

(defun wsp-chat--insert-pending ()
  "Insert the sends that have no final outcome yet."
  (dolist (pending wsp-chat--pending)
    (let ((start (point)))
      (insert (pcase (plist-get pending :state)
                ('sending (propertize (concat "  sending… " (plist-get pending :text))
                                      'face 'wsp-pending-face))
                ('uncertain
                 (concat (propertize "  may have been sent (check your phone; x dismisses): "
                                     'face 'wsp-error-face)
                         (plist-get pending :text))))
              "\n")
      (add-text-properties start (point)
                           (list 'wsp-pending (plist-get pending :op)
                                 'keymap wsp-chat-message-map
                                 'read-only t 'front-sticky '(read-only))))))

(defun wsp-chat--insert-prompt ()
  "Insert the prompt before the input area."
  (insert (propertize wsp-prompt 'face 'wsp-prompt-face
                      'read-only t 'front-sticky '(read-only) 'rear-nonsticky t)))

;;;; Loading more and moving

(defun wsp-chat-load-more ()
  "Show `wsp-history-page-size' more of the chat's older messages."
  (interactive)
  (let ((older (wsp-db-page-boundary wsp-chat-key wsp-chat--since wsp-history-page-size)))
    (if (not older)
        (message "All of this chat's history is shown")
      (setq wsp-chat--since older)
      (wsp-chat--redraw))))

(defun wsp-chat-show-message (id)
  "Load history back to message ID, put point at its start and highlight it.
Return non-nil if the message is in the buffer."
  (when-let* ((msg (wsp-db-message wsp-chat-key id)))
    (let ((boundary (wsp-db-page-boundary wsp-chat-key (plist-get msg :ts) 3)))
      (when (or (null wsp-chat--since)
                (> wsp-chat--since (or boundary (plist-get msg :ts))))
        (setq wsp-chat--since (or boundary (plist-get msg :ts)))
        (wsp-chat--redraw)))
    (when-let* ((start (wsp-chat--message-start id)))
      (goto-char start)
      (dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window start))
      (wsp-chat--highlight start)
      t)))

(defun wsp-chat--highlight (start)
  "Highlight the message starting at START until the next command."
  (when wsp-chat--target-overlay
    (delete-overlay wsp-chat--target-overlay))
  (let ((end (or (next-single-property-change start 'wsp-id) wsp-chat--input-marker)))
    (setq wsp-chat--target-overlay (make-overlay start end))
    (overlay-put wsp-chat--target-overlay 'face 'wsp-target-face)
    (let ((buffer (current-buffer)))
      (run-with-idle-timer
       0 nil
       (lambda ()
         (add-hook 'pre-command-hook (wsp-chat--unhighlighter buffer)))))))

(defun wsp-chat--unhighlighter (buffer)
  "Return a function that removes BUFFER's highlight and then itself."
  (letrec ((fn (lambda ()
                 (remove-hook 'pre-command-hook fn)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when wsp-chat--target-overlay
                       (delete-overlay wsp-chat--target-overlay)
                       (setq wsp-chat--target-overlay nil)))))))
    fn))

(defun wsp-chat-next-message ()
  "Move to the start of the next message."
  (interactive)
  (let ((pos (next-single-property-change (point) 'wsp-id)))
    (when (and pos (not (get-text-property pos 'wsp-id)))
      (setq pos (next-single-property-change pos 'wsp-id)))
    (goto-char (if (and pos (get-text-property pos 'wsp-id)) pos wsp-chat--input-marker))))

(defun wsp-chat-previous-message ()
  "Move to the start of the previous message."
  (interactive)
  (let* ((here (if (get-text-property (point) 'wsp-id)
                   (or (previous-single-property-change (1+ (point)) 'wsp-id) (point-min))
                 (point)))
         (pos (previous-single-property-change here 'wsp-id)))
    (while (and pos (> pos (point-min)) (not (get-text-property pos 'wsp-id)))
      (setq pos (previous-single-property-change pos 'wsp-id)))
    (when (and pos (get-text-property pos 'wsp-id))
      (goto-char (or (and (> pos (point-min))
                          (equal (get-text-property (1- pos) 'wsp-id)
                                 (get-text-property pos 'wsp-id))
                          (previous-single-property-change pos 'wsp-id))
                     pos)))))

;;;; Marking read

(defvar-local wsp-chat--mark-read-stuck nil
  "The unread count a mark-read left unchanged, or nil.
While the chat's unread count stays at this value, wsp does not ask
again, so messages the daemon cannot mark do not cause a request on
every redraw.")

(defun wsp-chat--visible-to-user-p ()
  "Return non-nil if this buffer is in the selected window of a focused frame.
A frame whose focus Emacs cannot tell, such as a terminal without focus
reporting, does not count."
  (and (eq (window-buffer (selected-window)) (current-buffer))
       (eq (frame-focus-state (selected-frame)) t)))

(defun wsp-chat--schedule-mark-read ()
  "Mark this chat read soon, if it is in front of the user and has unread messages."
  (let ((buffer (current-buffer)))
    (run-with-timer 1 nil (lambda () (wsp-chat--maybe-mark-read buffer)))))

(defun wsp-chat--maybe-mark-read (buffer)
  "Mark chat BUFFER read if it is in front of the user and has unread messages."
  (when (and wsp-mark-read (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((unread (and (not wsp-chat--marking-read)
                         (wsp-api-connected-p)
                         (wsp-chat--visible-to-user-p)
                         (wsp-db-unread-count wsp-chat-key))))
        (when (and unread (> unread 0) (not (eql unread wsp-chat--mark-read-stuck)))
          (setq wsp-chat--marking-read t)
          (wsp-api-request
           "mark_read" (list :chat wsp-chat-key)
           (lambda (_result error)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq wsp-chat--marking-read nil)
                 (let ((left (wsp-db-unread-count wsp-chat-key)))
                   (setq wsp-chat--mark-read-stuck (and (> left 0) left)))))
             (when error
               (let ((inhibit-message t))
                 (message "wsp: marking read failed: %s" (plist-get error :message)))))))))))

(defun wsp-chat-mark-read-visible (&rest _)
  "Mark read the chat in the selected window, if it is a chat buffer.
For `window-selection-change-functions' and focus changes."
  (let ((buffer (window-buffer (selected-window))))
    (when (with-current-buffer buffer (derived-mode-p 'wsp-chat-mode))
      (wsp-chat--maybe-mark-read buffer))))

;;;; Input and sending

(defun wsp-chat-return ()
  "Send the input when point is in the input area; otherwise open what is at point."
  (interactive)
  (if (>= (point) wsp-chat--input-marker)
      (wsp-chat-send-input)
    (wsp-chat-open-at-point)))

(defun wsp-chat-send-input ()
  "Send the input to this chat, as a reply or an edit if one is in progress."
  (interactive)
  (let ((text (string-trim (wsp-chat--input-text))))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (if wsp-chat--edit-target
        (wsp-chat--act "edit" (list :id (plist-get wsp-chat--edit-target :id) :text text) text)
      (wsp-chat--act "send"
                     (append (list :text text)
                             (when wsp-chat--reply-to
                               (list :reply_to (plist-get wsp-chat--reply-to :id))))
                     text))
    (wsp-chat--set-input "")
    (setq wsp-chat--reply-to nil
          wsp-chat--edit-target nil)
    (wsp-chat--typing-stopped)))

(defun wsp-chat--act (method params text)
  "Send acting METHOD with PARAMS for this chat, shown as TEXT while pending.
The outcome is one of three.  Sent: the daemon stored it, and the
redraw shows it.  Failed: nothing went out; a message or edit goes
back to the input, with its reply or edit target.  Uncertain: it may
have gone out; TEXT stays below the history, marked, and is never sent
again on its own, until the daemon says what became of it."
  (let* ((id (wsp-api-new-op))
         (buffer (current-buffer))
         (pending (list :op id :text text :state 'sending :method method
                        :reply-to wsp-chat--reply-to :edit-target wsp-chat--edit-target)))
    (setq wsp-chat--pending (append wsp-chat--pending (list pending)))
    (condition-case err
        (wsp-api-request
         method (append (list :op id :chat wsp-chat-key) params)
         (lambda (result error)
           (wsp-chat--settle buffer id result error))
         :timeout (if (equal method "send_file") 600 wsp-api-timeout))
      (user-error
       (setq wsp-chat--pending (delq pending wsp-chat--pending))
       (wsp-chat--redraw)
       (signal (car err) (cdr err))))
    (wsp-chat--redraw)))

(defun wsp-chat--pending-op (id)
  "Return the pending send with operation ID in this buffer, or nil."
  (seq-find (lambda (p) (equal (plist-get p :op) id)) wsp-chat--pending))

(defun wsp-chat--settle (buffer id result error)
  "Record in BUFFER the outcome of operation ID.
RESULT and ERROR are the API's answer.  An answer for an operation no
longer pending, such as one already settled, is ignored."
  (ignore result)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((pending (wsp-chat--pending-op id)))
        (let ((code (plist-get error :code)))
          (cond
           ((null error) (wsp-chat--drop-pending id))
           ((member code '("failed" "bad_request" "not_found" "not_connected"))
            (wsp-chat--fail pending (plist-get error :message)))
           (t (plist-put pending :state 'uncertain)
              (wsp-chat--check-op buffer id))))
        (wsp-chat--redraw)))))

(defun wsp-chat--fail (pending reason)
  "Settle PENDING as not sent because of REASON.
A message or edit goes back to the input with its reply or edit
target, unless the input holds something else."
  (wsp-chat--drop-pending (plist-get pending :op))
  (if (and (member (plist-get pending :method) '("send" "edit"))
           (string-empty-p (string-trim (wsp-chat--input-text))))
      (progn
        (wsp-chat--set-input (plist-get pending :text))
        (setq wsp-chat--reply-to (plist-get pending :reply-to)
              wsp-chat--edit-target (plist-get pending :edit-target))
        (message "wsp: not sent (it is back in the input): %s" reason))
    (message "wsp: not sent: %s" reason)))

(defun wsp-chat--drop-pending (id)
  "Forget the pending send with operation ID."
  (setq wsp-chat--pending
        (seq-remove (lambda (p) (equal (plist-get p :op) id)) wsp-chat--pending)))

(defun wsp-chat--check-op (buffer id)
  "Ask the daemon what became of operation ID, pending in BUFFER, and settle it."
  (when (wsp-api-connected-p)
    (ignore-errors
      (wsp-api-request
       "op_status" (list :op id)
       (lambda (result error)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when-let* ((pending (wsp-chat--pending-op id)))
               (pcase (plist-get result :state)
                 ("sent" (wsp-chat--drop-pending id) (wsp-chat--redraw))
                 ("failed" (wsp-chat--fail pending (or (plist-get result :error) "failed"))
                  (wsp-chat--redraw))
                 (_ (when (equal (plist-get error :code) "not_found")
                      ;; The daemon never journaled it, so nothing went out.
                      (wsp-chat--fail pending "wa-archive never received it")
                      (wsp-chat--redraw))))))))))))

(defun wsp-chat-recheck-uncertain ()
  "Ask the daemon about every uncertain send in every chat buffer."
  (dolist (buffer (wsp-chat-buffers))
    (with-current-buffer buffer
      (dolist (pending wsp-chat--pending)
        (when (eq (plist-get pending :state) 'uncertain)
          (wsp-chat--check-op buffer (plist-get pending :op)))))))

(defun wsp-chat--set-input (text)
  "Replace the input with TEXT."
  (let ((inhibit-read-only t))
    (delete-region wsp-chat--input-marker (point-max))
    (goto-char (point-max))
    (insert text)))

(defun wsp-chat-cancel ()
  "Cancel the reply or edit in progress."
  (interactive)
  (when wsp-chat--edit-target
    (wsp-chat--set-input ""))
  (setq wsp-chat--reply-to nil
        wsp-chat--edit-target nil)
  (force-mode-line-update)
  (message "Cancelled"))

;;;; Typing

(defun wsp-chat--after-change (beg _end _len)
  "Announce typing when the input changes at BEG and `wsp-send-typing' is on."
  (when (and wsp-send-typing wsp-chat--input-marker
             (>= beg wsp-chat--input-marker)
             (not inhibit-read-only)
             (wsp-api-connected-p))
    (when (or (null wsp-chat--typing-sent)
              (> (- (float-time) wsp-chat--typing-sent) 10))
      (setq wsp-chat--typing-sent (float-time))
      (ignore-errors
        (wsp-api-request "typing" (list :chat wsp-chat-key :state "composing") #'ignore)))
    (when wsp-chat--typing-timer (cancel-timer wsp-chat--typing-timer))
    (let ((buffer (current-buffer)))
      (setq wsp-chat--typing-timer
            (run-with-timer 5 nil (lambda ()
                                    (when (buffer-live-p buffer)
                                      (with-current-buffer buffer
                                        (wsp-chat--typing-stopped)))))))))

(defun wsp-chat--typing-stopped ()
  "Announce that typing stopped, if it was announced."
  (when wsp-chat--typing-timer
    (cancel-timer wsp-chat--typing-timer)
    (setq wsp-chat--typing-timer nil))
  (when (and wsp-chat--typing-sent (wsp-api-connected-p))
    (ignore-errors
      (wsp-api-request "typing" (list :chat wsp-chat-key :state "paused") #'ignore)))
  (setq wsp-chat--typing-sent nil))

;;;; Drafts

(defun wsp-chat--read-drafts ()
  "Return the saved drafts as an alist from chat key to text.
Signal an error if the file exists but cannot be read as one, so that
saving does not replace the other chats' drafts with nothing."
  (when (file-exists-p wsp-drafts-file)
    (let ((drafts (with-temp-buffer
                    (insert-file-contents wsp-drafts-file)
                    (unless (string-blank-p (buffer-string))
                      (condition-case nil
                          (read (current-buffer))
                        (error 'unreadable))))))
      (unless (and (listp drafts) (seq-every-p #'consp drafts))
        (error "Unreadable drafts file %s; not touching it" wsp-drafts-file))
      drafts)))

(defun wsp-chat--save-draft ()
  "Save this buffer's input as its chat's draft, or drop the draft if empty.
A failure is logged, not signaled, since this runs when a buffer is
killed and when Emacs exits."
  (when wsp-chat-key
    (condition-case err
        (let* ((text (string-trim (wsp-chat--input-text)))
               (drafts (assoc-delete-all wsp-chat-key (wsp-chat--read-drafts))))
          (unless (string-empty-p text)
            (push (cons wsp-chat-key text) drafts))
          (when (or drafts (file-exists-p wsp-drafts-file))
            (with-file-modes #o600
              (with-temp-file wsp-drafts-file
                (let ((print-length nil) (print-level nil))
                  (prin1 drafts (current-buffer)))))))
      (error (message "wsp: draft not saved: %s" (error-message-string err))))))

(defun wsp-chat-save-all-drafts ()
  "Save the input of every chat buffer as its draft."
  (dolist (buffer (wsp-chat-buffers))
    (with-current-buffer buffer
      (wsp-chat--save-draft))))

(defun wsp-chat--restore-draft ()
  "Put this chat's saved draft, if any, in the input."
  (when-let* ((text (cdr (assoc wsp-chat-key (wsp-chat--read-drafts)))))
    (wsp-chat--set-input text)))

(provide 'wsp-chat)
;;; wsp-chat.el ends here
