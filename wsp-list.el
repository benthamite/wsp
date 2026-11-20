;;; wsp-list.el --- The chat list of wsp  -*- lexical-binding: t; -*-

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

;; The buffer *wsp* lists every chat that has messages, most recent
;; first, with its unread count, the time of its latest message and a
;; preview of it.

;;; Code:

(require 'subr-x)
(require 'tabulated-list)
(require 'wsp-db)
(require 'wsp-media)
(require 'wsp-chat)

(declare-function wsp-chat "wsp")
(declare-function wsp-search "wsp-search")
(defvar wsp-health-warning)

(defconst wsp-list-buffer-name "*wsp*"
  "Name of the chat list buffer.")

(defface wsp-list-unread-face '((t :weight bold))
  "Face of the name of a chat with unread messages."
  :group 'wsp)

(defface wsp-list-count-face '((t :inherit success :weight bold))
  "Face of unread counts."
  :group 'wsp)

(defvar wsp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'wsp-list-open)
    (define-key map (kbd "c") #'wsp-chat)
    (define-key map (kbd "s") #'wsp-search)
    map)
  "Keymap of the chat list.")

(define-derived-mode wsp-list-mode tabulated-list-mode "wsp chats"
  "Major mode of the wsp chat list.

\\{wsp-list-mode-map}"
  (setq tabulated-list-format
        [("Chat" 30 t) ("New" 4 t :right-align t) ("When" 12 t) ("Last message" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local header-line-format
              '(:eval (if (bound-and-true-p wsp-health-warning)
                          (propertize (concat " " wsp-health-warning) 'face 'error)
                        (tabulated-list-init-header))))
  (setq-local revert-buffer-function (lambda (&rest _) (wsp-list-refresh)))
  (tabulated-list-init-header))

(defun wsp-list-buffer ()
  "Return the chat list buffer if it exists."
  (get-buffer wsp-list-buffer-name))

(defun wsp-list-refresh ()
  "Redraw the chat list from the store, keeping point on the same chat."
  (when-let* ((buffer (wsp-list-buffer)))
    (with-current-buffer buffer
      (let ((key (tabulated-list-get-id))
            (line (line-number-at-pos)))
        (setq tabulated-list-entries (mapcar #'wsp-list--entry (wsp-db-chats)))
        (tabulated-list-print t)
        (unless (and key (wsp-list--goto key))
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun wsp-list--goto (key)
  "Move to the row of chat KEY and return non-nil, or return nil."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (equal (tabulated-list-get-id) key)
          (setq found t)
        (forward-line 1)))
    found))

(defun wsp-list--entry (chat)
  "Return the tabulated list entry of CHAT, a plist from `wsp-db-chats'."
  (let* ((unread (plist-get chat :unread))
         (last (plist-get chat :last))
         (name (wsp-db-display-name chat)))
    (list (plist-get chat :key)
          (vector (propertize name 'face (if (> unread 0) 'wsp-list-unread-face 'default))
                  (if (> unread 0) (propertize (number-to-string unread) 'face 'wsp-list-count-face) "")
                  (propertize (wsp-list--time (plist-get last :ts)) 'face 'wsp-timestamp-face)
                  (wsp-list--preview last (plist-get chat :is-group))))))

(defun wsp-list--time (ts)
  "Return Unix time TS formatted for the list."
  (cond ((equal (format-time-string "%F" ts) (format-time-string "%F")) (format-time-string "%H:%M" ts))
        ((< (- (float-time) ts) (* 6 86400)) (format-time-string "%a %H:%M" ts))
        ((equal (format-time-string "%Y" ts) (format-time-string "%Y")) (format-time-string "%e %b" ts))
        (t (format-time-string "%F" ts))))

(defun wsp-list--preview (msg group)
  "Return a one-line preview of MSG; in a GROUP name its sender."
  (let* ((body (cond ((plist-get msg :revoked) "[deleted]")
                     ((let ((text (plist-get msg :text)))
                        (and text (not (string-empty-p text))
                             (replace-regexp-in-string "[\n\t]+" " " text))))
                     (t (wsp-media-label msg))))
         (who (cond ((plist-get msg :from-me) "Me: ")
                    (group (concat (wsp-db-sender-name msg) ": "))
                    (t ""))))
    (truncate-string-to-width (concat who body) 70 nil nil "…")))

(defun wsp-list-open ()
  "Open the chat on this line."
  (interactive)
  (if-let* ((key (tabulated-list-get-id)))
      (wsp-chat-open key)
    (user-error "No chat on this line")))

(defun wsp-list-show ()
  "Display the chat list, creating it if needed, and return its buffer."
  (let ((buffer (get-buffer-create wsp-list-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'wsp-list-mode)
        (wsp-list-mode)))
    (wsp-list-refresh)
    (pop-to-buffer-same-window buffer)
    buffer))

(provide 'wsp-list)
;;; wsp-list.el ends here
