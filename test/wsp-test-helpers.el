;;; wsp-test-helpers.el --- Fixtures for wsp's tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; A temporary wa-archive store with the daemon's schema (copied from
;; wa-archive's archive.go, `schema' and `clientSchema') holding invented chats, people and texts,
;; and a fake wa-archive API: a Unix socket server in the test Emacs
;; that records each request and answers it as the test says.  Nothing
;; here opens the real store or talks to the real daemon.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'wsp)

(defconst wsp-test-schema
  "CREATE TABLE chats (key TEXT PRIMARY KEY, name TEXT NOT NULL DEFAULT '',
     is_group INTEGER NOT NULL DEFAULT 0, updated_at INTEGER NOT NULL DEFAULT 0);
   CREATE TABLE aliases (alias TEXT PRIMARY KEY, key TEXT NOT NULL);
   CREATE TABLE messages (chat_key TEXT NOT NULL, id TEXT NOT NULL,
     sender_key TEXT NOT NULL DEFAULT '', sender_name TEXT NOT NULL DEFAULT '',
     from_me INTEGER NOT NULL DEFAULT 0, ts INTEGER NOT NULL,
     text TEXT NOT NULL DEFAULT '', quoted_id TEXT NOT NULL DEFAULT '',
     media_type TEXT NOT NULL DEFAULT '', file_name TEXT NOT NULL DEFAULT '',
     source TEXT NOT NULL, PRIMARY KEY (chat_key, id));
   CREATE INDEX messages_ts ON messages (chat_key, ts);
   CREATE TABLE mutations (chat_key TEXT NOT NULL, target_id TEXT NOT NULL,
     event_id TEXT NOT NULL, kind TEXT NOT NULL CHECK (kind IN ('edit', 'revoke')),
     ts INTEGER NOT NULL, text TEXT NOT NULL DEFAULT '',
     PRIMARY KEY (chat_key, target_id, event_id));
   CREATE TABLE reactions (chat_key TEXT NOT NULL, target_id TEXT NOT NULL,
     sender_key TEXT NOT NULL, from_me INTEGER NOT NULL DEFAULT 0,
     emoji TEXT NOT NULL, ts INTEGER NOT NULL,
     PRIMARY KEY (chat_key, target_id, sender_key));
   CREATE TABLE receipts (chat_key TEXT NOT NULL, message_id TEXT NOT NULL,
     reader_key TEXT NOT NULL,
     kind TEXT NOT NULL CHECK (kind IN ('delivered', 'read', 'played')),
     ts INTEGER NOT NULL, PRIMARY KEY (chat_key, message_id, reader_key, kind));
   CREATE TABLE chat_reads (chat_key TEXT PRIMARY KEY, watermark_ts INTEGER NOT NULL);
   CREATE TABLE read_messages (chat_key TEXT NOT NULL, id TEXT NOT NULL,
     PRIMARY KEY (chat_key, id));"
  "wa-archive's store schema, the parts wsp reads.")

(defconst wsp-test-alice "15550000001@s.whatsapp.net")
(defconst wsp-test-bob "15550000002@s.whatsapp.net")
(defconst wsp-test-carol "15550000003@s.whatsapp.net")
(defconst wsp-test-alice-lid "90000000001@lid")
(defconst wsp-test-group "120000000000000001@g.us")

(defvar wsp-test-db nil
  "A read-write connection to the fixture store.")

(defvar wsp-test-dir nil
  "The temporary directory of the current test.")

(defvar wsp-test-requests nil
  "Requests the fake API received, newest first, as plists.")

(defvar wsp-test-handler nil
  "Function of METHOD and PARAMS returning the fake API's answer.
It returns (:result VALUE), (:error CODE), or :none for no answer.")

(defvar wsp-test-server nil
  "The fake API server process.")

(defvar wsp-test-clients nil
  "Connections to the fake API server.")

(defun wsp-test-exec (sql &rest params)
  "Run SQL with PARAMS on the fixture store."
  (sqlite-execute wsp-test-db sql params))

(cl-defun wsp-test-chat (key &key (name "") group (updated 0))
  "Add chat KEY with NAME, a GROUP if non-nil, updated at UPDATED."
  (wsp-test-exec "INSERT INTO chats VALUES (?, ?, ?, ?)" key name (if group 1 0) updated))

(cl-defun wsp-test-message (chat id ts &key (sender "") (sender-name "") from-me (text "")
                                  (quoted "") (media "") (file "") (source "live"))
  "Add message ID to CHAT at TS with the given fields."
  (wsp-test-exec "INSERT INTO messages VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 chat id sender sender-name (if from-me 1 0) ts text quoted media file source))

(defun wsp-test-mutation (chat target kind ts &optional text)
  "Add a KIND mutation of TARGET in CHAT at TS with TEXT."
  (wsp-test-exec "INSERT INTO mutations VALUES (?, ?, ?, ?, ?, ?)"
                 chat target (format "ev-%s-%s" target ts) kind ts (or text "")))

(defmacro wsp-test-with-store (&rest body)
  "Run BODY with a fresh fixture store, state directory and no wsp session."
  (declare (indent 0) (debug t))
  `(let* ((wsp-test-dir (make-temp-file "wsp-test-" t))
          (wsp-data-directory (expand-file-name "data" wsp-test-dir))
          (wsp-state-directory (expand-file-name "state" wsp-test-dir))
          (wsp-drafts-file (expand-file-name "drafts.eld" wsp-test-dir))
          (wsp-api-socket nil)
          (wsp-mark-read nil)
          (wsp-notify nil)
          (wsp-auto-fetch-images nil)
          (wsp-send-presence nil)
          (wsp-send-typing nil)
          (wsp-api-timeout 30)
          (wsp-test-requests nil)
          (wsp-test-handler (lambda (_method _params) '(:result (:ok t))))
          (wsp-test-db nil))
     (make-directory wsp-data-directory t)
     (make-directory wsp-state-directory t)
     (setq wsp-test-db (sqlite-open (expand-file-name "messages.db" wsp-data-directory)))
     (dolist (statement (split-string wsp-test-schema ";" t "[ \n]+"))
       (sqlite-execute wsp-test-db statement))
     (unwind-protect
         (progn ,@body)
       (wsp-test--teardown))))

(defun wsp-test--teardown ()
  "Stop wsp, kill its buffers and delete the fixture."
  (ignore-errors (wsp-stop))
  (dolist (buffer (wsp-chat-buffers)) (kill-buffer buffer))
  (when (get-buffer wsp-list-buffer-name) (kill-buffer wsp-list-buffer-name))
  (when (get-buffer wsp-search-buffer-name) (kill-buffer wsp-search-buffer-name))
  (clrhash wsp-chat--buffers)
  (clrhash wsp-chat--fetch-requested)
  (setq wsp-notify--rowid nil wsp-health-warning nil)
  (wsp-test-stop-server)
  (wsp-db-close)
  (when wsp-test-db (sqlite-close wsp-test-db))
  (delete-directory wsp-test-dir t))

;;;; Fake API

(defun wsp-test-start-server ()
  "Start the fake API on the socket wsp expects."
  (setq wsp-test-clients nil)
  (setq wsp-test-server
        (make-network-process
         :name "wsp-test-server" :server t :family 'local :service (wsp-api-socket)
         :coding 'utf-8 :noquery t
         :log (lambda (_server client _message) (push client wsp-test-clients))
         :filter #'wsp-test--server-filter)))

(defun wsp-test-stop-server ()
  "Stop the fake API."
  (dolist (client wsp-test-clients) (ignore-errors (delete-process client)))
  (when (process-live-p wsp-test-server) (delete-process wsp-test-server))
  (setq wsp-test-server nil wsp-test-clients nil)
  (ignore-errors (delete-file (wsp-api-socket))))

(defun wsp-test--server-filter (process string)
  "Answer each request line in STRING from PROCESS."
  (let ((pending (concat (or (process-get process 'partial) "") string)))
    (while (string-match "\n" pending)
      (let* ((line (substring pending 0 (match-beginning 0)))
             (request (json-parse-string line :object-type 'plist :null-object nil
                                         :false-object nil)))
        (setq pending (substring pending (match-end 0)))
        (push request wsp-test-requests)
        (let ((answer (funcall wsp-test-handler (plist-get request :method)
                               (plist-get request :params))))
          (unless (eq answer :none)
            (process-send-string
             process
             (concat (json-serialize
                      (if (eq (car answer) :error)
                          (list :id (plist-get request :id)
                                :error (list :code (cadr answer) :message "test error"))
                        (list :id (plist-get request :id) :result (cadr answer))))
                     "\n"))))))
    (process-put process 'partial pending)))

(defun wsp-test-push-event (event)
  "Send EVENT, a plist, to every client of the fake API.
Wait first for the server to have accepted a client."
  (wsp-test-wait (lambda () wsp-test-clients))
  (dolist (client wsp-test-clients)
    (process-send-string client (concat (json-serialize event) "\n"))))

(defun wsp-test-wait (predicate &optional seconds)
  "Process input until PREDICATE returns non-nil or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun wsp-test-methods ()
  "Return the methods the fake API received, oldest first."
  (mapcar (lambda (r) (plist-get r :method)) (reverse wsp-test-requests)))

(defun wsp-test-connect ()
  "Start the fake API and connect wsp to it."
  (wsp-test-start-server)
  (wsp-api-start)
  (wsp-test-wait #'wsp-api-connected-p))

(defun wsp-test-buffer-text (buffer)
  "Return BUFFER's text without properties."
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

(provide 'wsp-test-helpers)
;;; wsp-test-helpers.el ends here
