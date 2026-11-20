;;; wsp-api.el --- Client for wa-archive's local API  -*- lexical-binding: t; -*-

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

;; wa-archive serves a JSON-lines API on a Unix socket (the contract is
;; docs/api.md).  This library connects to it, sends requests, routes
;; each answer to its callback, and runs `wsp-api-event-functions' on
;; the events the daemon pushes.  A Unix socket involves no network, so
;; connecting never waits on one.
;;
;; Every action on WhatsApp goes through here; reading goes through the
;; store (`wsp-db').  An acting request carries an operation id, so an
;; answer lost with the connection can be recovered with `op_status'
;; rather than guessed at: wsp never repeats an action on its own.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'wsp-db)

(defcustom wsp-api-socket nil
  "Path of wa-archive's API socket, or nil for api.sock in `wsp-data-directory'."
  :type '(choice (const :tag "In wsp-data-directory" nil) file)
  :group 'wsp)

(defcustom wsp-api-timeout 30
  "Seconds to wait for an answer before treating a request as unanswered."
  :type 'natnum
  :group 'wsp)

(defvar wsp-api-event-functions nil
  "Functions called with each event plist the daemon pushes.")

(defvar wsp-api-status-functions nil
  "Functions called with no arguments when the connection's state changes.")

(defvar wsp-api--process nil
  "The connection to the daemon, or nil.")

(defvar wsp-api--pending (make-hash-table)
  "Map from request id to (CALLBACK . TIMER).")

(defvar wsp-api--next-id 0
  "The id of the last request sent.")

(defvar wsp-api--partial ""
  "Received text not yet ending in a newline.")

(defvar wsp-api--reconnect-timer nil
  "Timer that retries connecting, or nil.")

(defvar wsp-api--wanted nil
  "Non-nil while wsp wants to stay connected.")

(defvar wsp-api--last-error nil
  "Why the last connection attempt failed, or nil.")

(defun wsp-api-socket ()
  "Return the path of the API socket."
  (or wsp-api-socket (expand-file-name "api.sock" wsp-data-directory)))

(defun wsp-api-connected-p ()
  "Return non-nil if wsp is connected to the daemon."
  (process-live-p wsp-api--process))

(defun wsp-api-last-error ()
  "Return why the last connection attempt failed, or nil."
  wsp-api--last-error)

(defun wsp-api-start ()
  "Connect to the daemon and keep reconnecting until `wsp-api-stop'."
  (setq wsp-api--wanted t)
  (wsp-api--connect))

(defun wsp-api-stop ()
  "Disconnect from the daemon and stop reconnecting."
  (setq wsp-api--wanted nil)
  (when wsp-api--reconnect-timer
    (cancel-timer wsp-api--reconnect-timer)
    (setq wsp-api--reconnect-timer nil))
  (when (process-live-p wsp-api--process)
    (delete-process wsp-api--process))
  (setq wsp-api--process nil))

(defun wsp-api--connect ()
  "Try to connect to the daemon; on failure, schedule another try."
  (unless (wsp-api-connected-p)
    (setq wsp-api--partial "")
    (condition-case err
        (progn
          (unless (file-exists-p (wsp-api-socket))
            (error "No API socket at %s; is wa-archive running?" (wsp-api-socket)))
          (setq wsp-api--process
                (make-network-process
                 :name "wsp-api" :family 'local :service (wsp-api-socket)
                 :coding 'utf-8 :noquery t
                 :filter #'wsp-api--filter :sentinel #'wsp-api--sentinel))
          (setq wsp-api--last-error nil)
          (run-hooks 'wsp-api-status-functions))
      (error
       (setq wsp-api--last-error (error-message-string err)
             wsp-api--process nil)
       (wsp-api--schedule-reconnect)))))

(defun wsp-api--schedule-reconnect ()
  "Retry connecting in a few seconds, while wsp wants a connection."
  (when (and wsp-api--wanted (not wsp-api--reconnect-timer))
    (setq wsp-api--reconnect-timer
          (run-with-timer 5 nil (lambda ()
                                  (setq wsp-api--reconnect-timer nil)
                                  (when wsp-api--wanted
                                    (wsp-api--connect)))))))

(defun wsp-api--sentinel (process _event)
  "Handle PROCESS closing: fail what was pending and reconnect later."
  (unless (process-live-p process)
    (when (eq process wsp-api--process)
      (setq wsp-api--process nil
            wsp-api--last-error "The connection to wa-archive closed")
      (wsp-api--fail-pending "disconnected" "The connection to wa-archive closed")
      (run-hooks 'wsp-api-status-functions)
      (wsp-api--schedule-reconnect))))

(defun wsp-api--fail-pending (code message)
  "Answer every pending request with error CODE and MESSAGE."
  (let (entries)
    (maphash (lambda (id entry) (push (cons id entry) entries)) wsp-api--pending)
    (clrhash wsp-api--pending)
    (pcase-dolist (`(,_id ,callback . ,timer) entries)
      (when timer (cancel-timer timer))
      (wsp-api--call callback nil (list :code code :message message)))))

(defun wsp-api--filter (_process string)
  "Split STRING into lines and handle each complete JSON message."
  (let ((lines (split-string (concat wsp-api--partial string) "\n")))
    (setq wsp-api--partial (car (last lines)))
    (dolist (line (butlast lines))
      (unless (string-empty-p (string-trim line))
        (wsp-api--handle line)))))

(defun wsp-api--handle (line)
  "Route the JSON message in LINE to its callback or the event hook."
  (let ((msg (condition-case nil
                 (json-parse-string line :object-type 'plist :array-type 'list
                                    :null-object nil :false-object nil)
               (error nil))))
    (cond
     ((null msg)
      (let ((inhibit-message t))
        (message "wsp: ignored a malformed line from wa-archive")))
     ((plist-get msg :event)
      (run-hook-with-args 'wsp-api-event-functions msg))
     ((plist-member msg :id)
      (let ((entry (gethash (plist-get msg :id) wsp-api--pending)))
        (when entry
          (remhash (plist-get msg :id) wsp-api--pending)
          (when (cdr entry) (cancel-timer (cdr entry)))
          (wsp-api--call (car entry) (plist-get msg :result) (plist-get msg :error))))))))

(defun wsp-api--call (callback result error)
  "Call CALLBACK with RESULT and ERROR, logging what it signals."
  (when callback
    (condition-case err
        (funcall callback result error)
      (error (let ((inhibit-message t))
               (message "wsp: error in API callback: %s" (error-message-string err)))))))

(cl-defun wsp-api-request (method params callback &key (timeout wsp-api-timeout))
  "Send METHOD with PARAMS (a plist) and call CALLBACK with the answer.
CALLBACK receives RESULT and ERROR; ERROR is nil on success, or a
plist with :code and :message.  If no answer comes within TIMEOUT
seconds, CALLBACK receives the error code \"timeout\".  Signal a
`user-error' if wsp is not connected."
  (unless (wsp-api-connected-p)
    (wsp-api--connect)
    (unless (wsp-api-connected-p)
      (user-error "Cannot reach wa-archive: %s"
                  (or wsp-api--last-error "not connected"))))
  (let* ((id (cl-incf wsp-api--next-id))
         (line (condition-case err
                   (concat (json-serialize (list :id id :method method
                                                 :params (or params (make-hash-table))))
                           "\n")
                 (error (user-error "Cannot encode the request: %s"
                                    (error-message-string err))))))
    (condition-case err
        (process-send-string wsp-api--process line)
      (error (user-error "Cannot reach wa-archive: %s" (error-message-string err))))
    (puthash id (cons callback (wsp-api--timeout-timer id callback timeout)) wsp-api--pending)
    id))

(defun wsp-api--timeout-timer (id callback timeout)
  "Return a timer that tells CALLBACK that request ID went unanswered.
It fires after TIMEOUT seconds.
The request stays registered, so an answer that arrives later still
reaches CALLBACK: an action thought uncertain can then be settled."
  (run-with-timer
   timeout nil
   (lambda ()
     (when-let* ((entry (gethash id wsp-api--pending)))
       (setcdr entry nil)
       (wsp-api--call callback nil
                      (list :code "timeout"
                            :message "wa-archive did not answer in time"))))))

(defun wsp-api-new-op ()
  "Return a fresh operation id for an acting request."
  (format "wsp-%s-%04x%04x" (format-time-string "%s%N") (random 65536) (random 65536)))

(provide 'wsp-api)
;;; wsp-api.el ends here
