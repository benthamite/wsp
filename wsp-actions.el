;;; wsp-actions.el --- Commands on messages for wsp  -*- lexical-binding: t; -*-

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

;; Commands on the message at point in a chat buffer: reply, edit,
;; delete for everyone, react, copy, open its attachment or link, and
;; attach a file.  Acting ones go through `wsp-chat--act', so each has
;; the same three outcomes as sending: done, failed, or uncertain.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'wsp-db)
(require 'wsp-media)
(require 'wsp-chat)

(defcustom wsp-reaction-choices '("👍" "❤️" "😂" "😮" "😢" "🙏")
  "Reactions offered first by `wsp-chat-react'."
  :type '(repeat string)
  :group 'wsp)

(defun wsp-chat--message-at-point ()
  "Return the message plist at point, or signal a `user-error'."
  (let ((id (get-text-property (point) 'wsp-id)))
    (or (and id (wsp-db-message wsp-chat-key id))
        (user-error "No message at point"))))

(defun wsp-chat--goto-input ()
  "Move point to the end of the input area."
  (goto-char (point-max)))

(defun wsp-chat-reply ()
  "Reply to the message at point with the next message you send."
  (interactive)
  (let ((msg (wsp-chat--message-at-point)))
    (setq wsp-chat--reply-to msg
          wsp-chat--edit-target nil)
    (wsp-chat--goto-input)
    (force-mode-line-update)
    (message "Replying to %s" (wsp-db-sender-name msg))))

(defun wsp-chat-edit ()
  "Edit the message at point, one of yours; the input then replaces it."
  (interactive)
  (let ((msg (wsp-chat--message-at-point)))
    (unless (plist-get msg :from-me)
      (user-error "You can only edit your own messages"))
    (when (plist-get msg :revoked)
      (user-error "That message was deleted"))
    (unless (or (string-empty-p (string-trim (wsp-chat--input-text)))
                (yes-or-no-p "Replace your unsent input with the message to edit? "))
      (user-error "Cancelled"))
    (setq wsp-chat--edit-target msg
          wsp-chat--reply-to nil)
    (wsp-chat--set-input (or (plist-get msg :text) ""))
    (force-mode-line-update)))

(defun wsp-chat-delete ()
  "Delete the message at point, one of yours, for everyone."
  (interactive)
  (let ((msg (wsp-chat--message-at-point)))
    (unless (plist-get msg :from-me)
      (user-error "You can only delete your own messages for everyone"))
    (when (plist-get msg :revoked)
      (user-error "That message is already deleted"))
    (when (yes-or-no-p "Delete this message for everyone? ")
      (wsp-chat--act "delete" (list :id (plist-get msg :id)) "[deleting]"))))

(defun wsp-chat-react (emoji)
  "React to the message at point with EMOJI; an empty EMOJI removes yours."
  (interactive
   (list (completing-read "React with (empty removes): " wsp-reaction-choices)))
  (let ((msg (wsp-chat--message-at-point)))
    (wsp-chat--act "react" (list :id (plist-get msg :id) :emoji emoji)
                   (if (string-empty-p emoji) "[removing reaction]" emoji))))

(defun wsp-chat-copy ()
  "Copy the text of the message at point."
  (interactive)
  (let ((text (plist-get (wsp-chat--message-at-point) :text)))
    (when (or (null text) (string-empty-p text))
      (user-error "That message has no text"))
    (kill-new text)
    (message "Copied")))

(defun wsp-chat-open-at-point ()
  "Open what is at point: a URL, or the message's attachment.
An attachment not yet downloaded is fetched first; a voice note or
audio is played, or stopped if playing."
  (interactive)
  (if-let* ((url (thing-at-point 'url)))
      (if (string-match-p "\\`\\(?:https?\\|mailto\\):" url)
          (browse-url url)
        (user-error "Not opening %s: only web and mail links are opened" url))
    (let* ((msg (wsp-chat--message-at-point))
           (file (wsp-media-file wsp-chat-key (plist-get msg :id))))
      (cond
       ((not (wsp-media-fetchable-p msg)) (user-error "Nothing to open here"))
       (file (wsp-actions--open-file msg file))
       (t (message "Downloading…")
          (wsp-chat--fetch msg (lambda (path)
                                 (wsp-actions--open-file msg path))))))))

(defcustom wsp-safe-extensions
  '("jpg" "jpeg" "png" "gif" "webp" "heic" "pdf" "txt" "mp4" "mov" "m4a"
    "mp3" "ogg" "opus" "wav" "docx" "xlsx" "pptx" "odt" "ods" "csv")
  "File extensions `wsp-chat-open-at-point' opens without asking.
Anything else, such as a .terminal, .app or .fileloc file a sender
named, which the system could run rather than show, is opened only
after you confirm."
  :type '(repeat string)
  :group 'wsp)

(defun wsp-actions--open-file (msg file)
  "Open FILE, the attachment of MSG: play audio, open anything else.
A file whose extension is not in `wsp-safe-extensions' is opened only
after confirmation."
  (cond
   ((member (plist-get msg :media-type) '("voice" "audio"))
    (wsp-media-play file))
   ((or (member (downcase (or (file-name-extension file) "")) wsp-safe-extensions)
        (yes-or-no-p (format "%s may be a program; open it anyway? "
                             (file-name-nondirectory file))))
    (wsp-media-open file))))

(defun wsp-chat-attach-file (file caption)
  "Send FILE to this chat with CAPTION, which may be empty."
  (interactive
   (list (read-file-name "Attach file: " nil nil t)
         (read-string "Caption (optional): ")))
  (let ((file (expand-file-name file)))
    (unless (file-regular-p file)
      (user-error "Not a file: %s" file))
    (wsp-chat--act "send_file"
                   (append (list :path file)
                           (unless (string-empty-p caption) (list :caption caption))
                           (when wsp-chat--reply-to
                             (list :reply_to (plist-get wsp-chat--reply-to :id))))
                   (format "[file: %s] %s" (file-name-nondirectory file) caption))
    (setq wsp-chat--reply-to nil)))

(defun wsp-chat-dismiss-pending ()
  "Dismiss the uncertain send at point, after checking it on your phone."
  (interactive)
  (let ((op (get-text-property (point) 'wsp-pending)))
    (unless op
      (user-error "No pending message at point"))
    (wsp-chat--drop-pending op)
    (wsp-chat--redraw)))

(provide 'wsp-actions)
;;; wsp-actions.el ends here
