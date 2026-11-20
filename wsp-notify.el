;;; wsp-notify.el --- Notifications and unread indicator for wsp  -*- lexical-binding: t; -*-

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

;; Desktop notifications for incoming messages, one per chat for each
;; batch that arrives, and the unread count in the mode line.  Like
;; sgn's, they use osascript on macOS and notifications.el elsewhere.
;; No notification is shown for the chat you are looking at.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'wsp-db)
(require 'wsp-media)
(require 'wsp-chat)
(require 'wsp-list)

(declare-function notifications-notify "notifications")

(defcustom wsp-notify t
  "Whether to show a desktop notification for incoming messages."
  :type 'boolean
  :group 'wsp)

(defcustom wsp-notify-show-text t
  "Whether notifications show the message text, or only who wrote."
  :type 'boolean
  :group 'wsp)

(defvar wsp-notify--rowid nil
  "The newest store rowid already considered for notifications, or nil.")

(defvar wsp-notify--mode-line-string ""
  "The unread indicator shown in the mode line.")
(put 'wsp-notify--mode-line-string 'risky-local-variable t)

(defun wsp-notify-check ()
  "Notify about live incoming messages stored since the last check.
The first check only records where the store is, so starting wsp
does not notify about old messages."
  (pcase-let ((`(,rowid . ,messages) (wsp-db-new-incoming (or wsp-notify--rowid 0))))
    (when (and wsp-notify wsp-notify--rowid messages)
      (dolist (group (seq-group-by (lambda (m) (plist-get m :chat)) messages))
        (unless (wsp-notify--looking-at-p (car group))
          (wsp-notify--show (car group) (cdr group)))))
    (setq wsp-notify--rowid rowid)))

(defun wsp-notify--looking-at-p (chat)
  "Return non-nil if CHAT's buffer is in the selected window of a focused frame."
  (let ((buffer (window-buffer (selected-window))))
    (and (frame-focus-state (selected-frame))
         (with-current-buffer buffer
           (and (derived-mode-p 'wsp-chat-mode)
                (equal wsp-chat-key (wsp-db-canonical chat)))))))

(defun wsp-notify--show (chat messages)
  "Show one notification for MESSAGES, new in CHAT."
  (let* ((info (or (wsp-db-chat chat) (list :key chat)))
         (last (car (last messages)))
         (sender (wsp-db-sender-name last))
         (title (if (plist-get info :is-group)
                    (format "%s in %s" sender (wsp-db-display-name info))
                  sender))
         (body (cond ((cdr messages) (format "%d new messages" (length messages)))
                     ((not wsp-notify-show-text) "New message")
                     (t (wsp-notify--preview last)))))
    (wsp-notify--desktop title body)))

(defun wsp-notify--preview (msg)
  "Return a short preview of MSG."
  (let ((text (plist-get msg :text)))
    (truncate-string-to-width
     (if (and text (not (string-empty-p text))) text (wsp-media-label msg))
     100 nil nil "…")))

(defun wsp-notify--desktop (title body)
  "Show a desktop notification with TITLE and BODY."
  (cond
   ((eq system-type 'darwin)
    (start-process "wsp-notify" nil "osascript" "-e"
                   (format "display notification %s with title %s"
                           (wsp-notify--applescript-quote body)
                           (wsp-notify--applescript-quote title))))
   ((and (eq system-type 'gnu/linux) (require 'notifications nil t))
    (notifications-notify :title title :body body :app-name "wsp"))
   (t (message "%s: %s" title body))))

(defun wsp-notify--applescript-quote (string)
  "Return STRING as an AppleScript string literal."
  (format "\"%s\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" string)))

(defun wsp-notify-update-indicator ()
  "Update the unread count shown in the mode line."
  (let ((total (wsp-db-unread-total)))
    (setq wsp-notify--mode-line-string
          (if (> total 0)
              (propertize (format " WA:%d" total) 'face 'wsp-list-count-face
                          'help-echo "Unread WhatsApp messages")
            ""))
    (force-mode-line-update t)))

;;;###autoload
(define-minor-mode wsp-indicator-mode
  "Show the number of unread WhatsApp messages in the mode line."
  :global t
  :group 'wsp
  (if wsp-indicator-mode
      (progn
        (unless (memq 'wsp-notify--mode-line-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(wsp-notify--mode-line-string))))
        (ignore-errors (wsp-notify-update-indicator)))
    (setq global-mode-string (delq 'wsp-notify--mode-line-string global-mode-string))
    (force-mode-line-update t)))

(provide 'wsp-notify)
;;; wsp-notify.el ends here
