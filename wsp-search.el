;;; wsp-search.el --- Searching messages in wsp  -*- lexical-binding: t; -*-

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

;; Search across all chats or within one.  Results are listed newest
;; first; RET opens a result's chat at that message.

;;; Code:

(require 'subr-x)
(require 'wsp-db)
(require 'wsp-media)
(require 'wsp-chat)

(declare-function wsp-open-message "wsp")

(defconst wsp-search-buffer-name "*wsp search*"
  "Name of the search results buffer.")

(defvar wsp-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'wsp-search-goto-result)
    (define-key map (kbd "n") #'wsp-search-next-result)
    (define-key map (kbd "p") #'wsp-search-previous-result)
    map)
  "Keymap of the search results buffer.")

(define-derived-mode wsp-search-mode special-mode "wsp search"
  "Major mode of wsp search results.

\\{wsp-search-mode-map}")

;;;###autoload
(defun wsp-search (query)
  "Search all chats for messages containing QUERY."
  (interactive (list (read-string "Search WhatsApp: ")))
  (wsp-search--show query nil))

;;;###autoload
(defun wsp-search-chat (query)
  "Search the current chat for messages containing QUERY."
  (interactive (list (read-string "Search this chat: ")))
  (unless (derived-mode-p 'wsp-chat-mode)
    (user-error "Not in a wsp chat buffer"))
  (wsp-search--show query wsp-chat-key))

(defun wsp-search--show (query chat)
  "Show the messages containing QUERY, in CHAT or in all chats."
  (when (string-empty-p (string-trim query))
    (user-error "Empty search"))
  (let ((results (wsp-db-search query chat))
        (buffer (get-buffer-create wsp-search-buffer-name)))
    (with-current-buffer buffer
      (wsp-search-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%d result%s for “%s”%s\n\n"
                                    (length results) (if (= (length results) 1) "" "s")
                                    query
                                    (if chat (format " in %s" (wsp-chat--name chat)) ""))
                            'face 'bold))
        (dolist (msg results)
          (wsp-search--insert msg query))
        (goto-char (point-min))
        (wsp-search-next-result)))
    (pop-to-buffer buffer)))

(defun wsp-search--insert (msg query)
  "Insert search result MSG, with QUERY highlighted."
  (let ((start (point))
        (text (replace-regexp-in-string
               "\n" " " (let ((text (plist-get msg :text)))
                          (if (and text (not (string-empty-p text))) text (wsp-media-label msg))))))
    (insert (propertize (wsp-chat--name (plist-get msg :chat)) 'face 'wsp-header-face)
            "  " (propertize (wsp-chat-format-time (plist-get msg :ts)) 'face 'wsp-timestamp-face)
            "  " (wsp-db-sender-name msg) ": "
            (wsp-search--highlight (truncate-string-to-width text 160 nil nil "…") query)
            "\n")
    (put-text-property start (point) 'wsp-search-ref
                       (concat (plist-get msg :chat) "/" (plist-get msg :id)))))

(defun wsp-search--highlight (text query)
  "Return TEXT with the occurrences of QUERY highlighted."
  (let ((case-fold-search t)
        (start 0))
    (setq text (copy-sequence text))
    (while (string-match (regexp-quote query) text start)
      (add-face-text-property (match-beginning 0) (match-end 0) 'match nil text)
      (setq start (match-end 0)))
    text))

(defun wsp-search-goto-result ()
  "Open the chat of the result at point at its message."
  (interactive)
  (if-let* ((ref (get-text-property (point) 'wsp-search-ref)))
      (wsp-open-message ref)
    (user-error "No result at point")))

(defun wsp-search-next-result ()
  "Move to the next result."
  (interactive)
  (let ((pos (if (get-text-property (point) 'wsp-search-ref)
                 (next-single-property-change (point) 'wsp-search-ref)
               (point))))
    (when-let* ((next (and pos (if (get-text-property pos 'wsp-search-ref)
                                   pos
                                 (next-single-property-change pos 'wsp-search-ref)))))
      (goto-char next))))

(defun wsp-search-previous-result ()
  "Move to the previous result."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'wsp-search-ref)))
    (when pos
      (unless (get-text-property pos 'wsp-search-ref)
        (setq pos (previous-single-property-change pos 'wsp-search-ref)))
      (when pos (goto-char pos)))))

(provide 'wsp-search)
;;; wsp-search.el ends here
