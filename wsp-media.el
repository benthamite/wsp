;;; wsp-media.el --- Media for wsp  -*- lexical-binding: t; -*-

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

;; Attachments as wa-archive keeps them.  The daemon downloads an
;; attachment into media/<safe chat key>/<safe id>-<safe name>, where
;; the name is the message's file name or, when it has none, the media
;; kind plus an extension from its MIME type; voice notes and audio get
;; a transcript next to the file, with ".txt" appended.  An alias merge
;; moves database rows but not files, so a file may sit under a former
;; alias's directory.  Files are therefore found by globbing for the
;; message id in the directories of the chat and of all its aliases.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'wsp-db)

(declare-function w32-shell-execute "w32fns.c")

(defcustom wsp-inline-images t
  "Whether to show downloaded images inline in chat buffers."
  :type 'boolean
  :group 'wsp)

(defcustom wsp-image-max-width 320
  "Maximum width in pixels of an inline image."
  :type 'natnum
  :group 'wsp)

(defcustom wsp-auto-fetch-images t
  "Whether to download images when a chat showing them is drawn.
Downloading is done by wa-archive, as the phone does on Wi-Fi."
  :type 'boolean
  :group 'wsp)

(defcustom wsp-audio-players '("afplay" "mpv" "ffplay")
  "Programs to play voice notes and audio with, in order of preference.
The first one found in the variable `exec-path' is used."
  :type '(repeat string)
  :group 'wsp)

(defconst wsp-media--fetchable '("image" "video" "audio" "voice" "document")
  "Media kinds wa-archive can download.")

(defconst wsp-media--labels
  '(("image" . "image") ("video" . "video") ("voice" . "voice message")
    ("audio" . "audio") ("document" . "document") ("sticker" . "sticker")
    ("deleted" . "deleted"))
  "Readable labels of wa-archive's media kinds.")

(defun wsp-media-safe-name (string)
  "Return STRING with characters wa-archive avoids in file names replaced.
This mirrors wa-archive's unsafeName: runs of characters other than
ASCII letters, digits, underscore, dot, hyphen and space become \"_\"."
  (replace-regexp-in-string "[^0-9A-Za-z_. -]+" "_" string))

(defun wsp-media-directory ()
  "Return the directory wa-archive downloads attachments into."
  (expand-file-name "media" wsp-data-directory))

(defvar wsp-media--listing nil
  "Cache of directory listings while a chat is drawn, or nil.
Bound to a hash table from directory to its files, so drawing a chat
lists each media directory once rather than once per attachment.")

(defun wsp-media--chat-directories (chat)
  "Return the media directories of CHAT and of its aliases."
  (let ((cached (and wsp-media--listing (gethash (cons 'dirs chat) wsp-media--listing))))
    (or cached
        (let ((dirs (mapcar (lambda (key)
                              (expand-file-name (wsp-media-safe-name key) (wsp-media-directory)))
                            (cons chat (wsp-db-aliases chat)))))
          (when wsp-media--listing
            (puthash (cons 'dirs chat) dirs wsp-media--listing))
          dirs))))

(defun wsp-media--candidates (chat id)
  "Return the files under CHAT's media directories whose names start with ID."
  (let ((prefix (concat (wsp-media-safe-name id) "-")))
    (seq-mapcat
     (lambda (dir)
       (seq-filter (lambda (file) (string-prefix-p prefix (file-name-nondirectory file)))
                   (wsp-media--files dir)))
     (wsp-media--chat-directories chat))))

(defun wsp-media--files (dir)
  "Return the files in DIR, from `wsp-media--listing' when it is bound."
  (let ((cached (and wsp-media--listing (gethash dir wsp-media--listing 'none))))
    (if (and cached (not (eq cached 'none)))
        cached
      (let ((files (and (file-directory-p dir) (directory-files dir t "\\`[^.]" t))))
        (when wsp-media--listing
          (puthash dir files wsp-media--listing))
        files))))

(defun wsp-media-file (chat id)
  "Return the downloaded attachment of message ID in CHAT, or nil."
  (car (sort (seq-remove (lambda (file) (string-suffix-p ".txt" file))
                         (wsp-media--candidates chat id))
             #'string<)))

(defun wsp-media-transcript (chat id)
  "Return the transcript of voice note or audio ID in CHAT, or nil."
  (when-let* ((file (car (sort (seq-filter (lambda (f) (string-suffix-p ".txt" f))
                                           (wsp-media--candidates chat id))
                               #'string<))))
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))

(defun wsp-media-directories-signature (chat)
  "Return a value that changes when CHAT's media directories change."
  (mapcar (lambda (dir)
            (file-attribute-modification-time (file-attributes dir)))
          (wsp-media--chat-directories chat)))

(defun wsp-media-fetchable-p (msg)
  "Return non-nil if wa-archive can download the attachment of MSG."
  (member (plist-get msg :media-type) wsp-media--fetchable))

(defun wsp-media-label (msg)
  "Return the bracketed label of MSG's attachment or special content."
  (let* ((kind (plist-get msg :media-type))
         (name (plist-get msg :file-name))
         (label (or (cdr (assoc kind wsp-media--labels)) kind)))
    (if (and name (not (string-empty-p name)))
        (format "[%s: %s]" label name)
      (format "[%s]" label))))

(defun wsp-media-system-p (msg)
  "Return non-nil if MSG is a WhatsApp record rather than content.
wa-archive stores content it does not understand under its protobuf
field name, such as messageHistoryNotice, with no text."
  (let ((kind (plist-get msg :media-type)))
    (and kind (not (string-empty-p kind))
         (not (assoc kind wsp-media--labels))
         (string-empty-p (or (plist-get msg :text) "")))))

(defun wsp-media-image-p (msg)
  "Return non-nil if MSG carries an image."
  (equal (plist-get msg :media-type) "image"))

(defun wsp-media-insert-image (file)
  "Insert FILE as an inline image, if this Emacs can show it."
  (when-let* (((display-images-p))
              (image (ignore-errors
                       (create-image file nil nil :max-width wsp-image-max-width))))
    (insert "  ")
    (insert-image image "[image]")
    (insert "\n")))

(defun wsp-media-open (file)
  "Open FILE with the system's default application."
  (pcase system-type
    ('darwin (call-process "open" nil 0 nil file))
    ('windows-nt (w32-shell-execute "open" file))
    (_ (call-process "xdg-open" nil 0 nil file))))

(defvar wsp-media--player nil
  "The process playing audio, or nil.")

(defun wsp-media-play (file)
  "Play audio FILE, or stop the audio playing now."
  (if (process-live-p wsp-media--player)
      (progn (delete-process wsp-media--player)
             (setq wsp-media--player nil)
             (message "Stopped"))
    (let ((player (seq-find #'executable-find wsp-audio-players)))
      (unless player
        (user-error "No audio player found; see `wsp-audio-players'"))
      (setq wsp-media--player
            (make-process :name "wsp-audio" :command (list player file)
                          :noquery t :buffer nil)))))

(provide 'wsp-media)
;;; wsp-media.el ends here
