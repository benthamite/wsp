;;; wsp-test.el --- Tests for wsp  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for wsp on fixture stores (wsp-test-helpers.el).  All
;; chats, people and texts are invented.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'wsp-test-helpers)

(defun wsp-test-populate ()
  "Fill the fixture store with a small invented world."
  (wsp-test-chat wsp-test-alice :name "Alice Example" :updated 1000)
  (wsp-test-chat wsp-test-bob :updated 900)
  (wsp-test-chat wsp-test-group :name "Book club" :group t :updated 1100)
  (wsp-test-chat wsp-test-carol :name "Carol NoMessages")
  (wsp-test-exec "INSERT INTO aliases VALUES (?, ?)" wsp-test-alice-lid wsp-test-alice)
  (wsp-test-message wsp-test-alice "A1" 100 :sender wsp-test-alice :text "hello there")
  (wsp-test-message wsp-test-alice "A2" 110 :from-me t :text "hi Alice" :source "sent")
  (wsp-test-message wsp-test-alice "A3" 120 :sender wsp-test-alice :text "typo here"
                    :quoted "A2")
  (wsp-test-mutation wsp-test-alice "A3" "edit" 125 "fixed text")
  (wsp-test-message wsp-test-alice "A4" 130 :sender wsp-test-alice :text "secret plan")
  (wsp-test-mutation wsp-test-alice "A4" "revoke" 131)
  (wsp-test-message wsp-test-alice "A5" 1000 :sender wsp-test-alice :media "image"
                    :file "cat.jpg")
  (wsp-test-message wsp-test-bob "B1" 900 :sender wsp-test-bob :sender-name "Bobby"
                    :text "from bob")
  (wsp-test-message wsp-test-group "G1" 1050 :sender wsp-test-alice :text "group hello")
  (wsp-test-message wsp-test-group "G2" 1100 :sender wsp-test-carol :sender-name "Caz"
                    :media "voice")
  (wsp-test-message wsp-test-group "G3" 1101 :media "messageHistoryNotice")
  (wsp-test-exec "INSERT INTO reactions VALUES (?, ?, ?, 0, ?, ?)"
                 wsp-test-alice "A2" wsp-test-alice "👍" 112)
  (wsp-test-exec "INSERT INTO reactions VALUES (?, ?, ?, 1, ?, ?)"
                 wsp-test-alice "A1" "me" "" 113)
  (wsp-test-exec "INSERT INTO receipts VALUES (?, ?, ?, 'read', ?)"
                 wsp-test-alice "A2" wsp-test-alice 115)
  (wsp-test-exec "INSERT INTO chat_reads VALUES (?, ?)" wsp-test-alice 100)
  (wsp-test-exec "INSERT INTO read_messages VALUES (?, ?)" wsp-test-alice "A3"))

;;;; Store

(ert-deftest wsp-test-schema-check-names-missing-tables ()
  (wsp-test-with-store
    (wsp-test-exec "DROP TABLE reactions")
    (let ((err (should-error (wsp-db) :type 'wsp-db-error)))
      (should (string-match-p "reactions" (cadr err))))))

(ert-deftest wsp-test-store-is-read-only ()
  (wsp-test-with-store
    (wsp-test-populate)
    (should (equal (caar (wsp-db-select "SELECT COUNT(*) FROM chats")) 4))
    (should-error (sqlite-execute (wsp-db) "DELETE FROM chats"))
    (should (equal (caar (wsp-db-select "SELECT COUNT(*) FROM chats")) 4))))

(ert-deftest wsp-test-chats-order-names-and-unread ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((chats (wsp-db-chats)))
      (should (equal (mapcar (lambda (c) (plist-get c :key)) chats)
                     (list wsp-test-group wsp-test-alice wsp-test-bob)))
      (should (equal (mapcar #'wsp-db-display-name chats)
                     '("Book club" "Alice Example" "Bobby")))
      ;; Alice: A1 at the watermark, A3 read by id, A4 deleted, A5 unread.
      (should (equal (plist-get (nth 1 chats) :unread) 1))
      (should (equal (plist-get (nth 2 chats) :unread) 1))
      (should (equal (wsp-db-unread-total) 4)))))

(ert-deftest wsp-test-unread-respects-tied-timestamps ()
  (wsp-test-with-store
    (wsp-test-chat wsp-test-alice)
    (wsp-test-message wsp-test-alice "X1" 500 :sender wsp-test-alice :text "a")
    (wsp-test-message wsp-test-alice "X2" 500 :sender wsp-test-alice :text "b")
    (wsp-test-exec "INSERT INTO chat_reads VALUES (?, 499)" wsp-test-alice)
    (wsp-test-exec "INSERT INTO read_messages VALUES (?, 'X1')" wsp-test-alice)
    (should (equal (wsp-db-unread-count wsp-test-alice) 1))))

(ert-deftest wsp-test-message-edits-and-revokes ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((edited (wsp-db-message wsp-test-alice "A3"))
          (revoked (wsp-db-message wsp-test-alice "A4")))
      (should (equal (plist-get edited :text) "fixed text"))
      (should (plist-get edited :edited))
      (should (plist-get revoked :revoked)))))

(ert-deftest wsp-test-sender-names ()
  (wsp-test-with-store
    (wsp-test-populate)
    (should (equal (wsp-db-sender-name (wsp-db-message wsp-test-group "G1")) "Alice Example"))
    (should (equal (wsp-db-sender-name (wsp-db-message wsp-test-group "G2")) "Carol NoMessages"))
    (should (equal (wsp-db-sender-name (wsp-db-message wsp-test-alice "A2")) "Me"))
    (should (equal (wsp-db-jid-number wsp-test-bob) "+15550000002"))))

;;;; Chat list

(ert-deftest wsp-test-list-shows-chats-with-previews ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((text (wsp-test-buffer-text (wsp-list-show))))
      (should (string-match-p "Book club" text))
      (should (string-match-p "Book club.*\\[messageHistoryNotice\\]" text))
      (should (string-match-p "Alice Example.*\\[image: cat.jpg\\]" text))
      (should (string-match-p "Bobby.*from bob" text))
      (should-not (string-match-p "Carol NoMessages" text)))))

;;;; Chat view

(ert-deftest wsp-test-chat-renders-messages ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((text (wsp-test-buffer-text (wsp-chat-buffer wsp-test-alice))))
      (should (string-match-p "── Alice Example · " text))
      (should (string-match-p "hello there" text))
      (should (string-match-p "── Me · " text))
      (should (string-match-p "hi Alice ✓✓" text))
      (should (string-match-p "👍" text))
      (should (string-match-p "┃ Me: hi Alice" text))
      (should (string-match-p "fixed text (edited)" text))
      (should-not (string-match-p "typo here" text))
      (should (string-match-p "\\[deleted\\]" text))
      (should-not (string-match-p "secret plan" text))
      (should (string-match-p "\\[image: cat.jpg\\] (not downloaded; RET fetches)" text))
      (should (string-suffix-p wsp-prompt text)))))

(ert-deftest wsp-test-chat-buffer-for-alias-is-the-canonical-one ()
  (wsp-test-with-store
    (wsp-test-populate)
    (should (eq (wsp-chat-buffer wsp-test-alice-lid) (wsp-chat-buffer wsp-test-alice)))))

(ert-deftest wsp-test-group-chat-voice-transcript-and-system-record ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((dir (expand-file-name (wsp-media-safe-name wsp-test-group) (wsp-media-directory))))
      (make-directory dir t)
      (with-temp-file (expand-file-name "G2-voice.ogg" dir) (insert "x"))
      (with-temp-file (expand-file-name "G2-voice.ogg.txt" dir) (insert "invented words\n")))
    (let ((text (wsp-test-buffer-text (wsp-chat-buffer wsp-test-group))))
      (should (string-match-p "── Carol NoMessages · " text))
      (should (string-match-p "\\[voice message\\] (RET plays)" text))
      (should (string-match-p "“invented words”" text))
      (should (string-match-p "\\[messageHistoryNotice\\]" text))
      (should-not (string-match-p "”\n\n" text)))))

(ert-deftest wsp-test-own-media-without-text-carries-its-mark ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-message wsp-test-bob "B7" 950 :from-me t :media "document" :file "notes.pdf"
                      :source "sent")
    (let ((text (wsp-test-buffer-text (wsp-chat-buffer wsp-test-bob))))
      (should (string-match-p "\\[document: notes.pdf\\] (not downloaded; RET fetches) ✓\n" text)))))

(ert-deftest wsp-test-reply-header-names-the-message ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-chat-buffer wsp-test-alice)
      (goto-char (wsp-chat--message-start "A1"))
      (wsp-chat-reply)
      (should (string-match-p "replying to Alice Example: hello there"
                              (wsp-chat--header-line))))))

(ert-deftest wsp-test-paging-loads-older-messages ()
  (wsp-test-with-store
    (wsp-test-chat wsp-test-bob)
    (dotimes (i 10)
      (wsp-test-message wsp-test-bob (format "M%02d" i) (* 10 (1+ i))
                        :sender wsp-test-bob :text (format "message number %02d" i)))
    (let* ((wsp-history-page-size 4)
           (buffer (wsp-chat-buffer wsp-test-bob)))
      (should-not (string-match-p "number 05" (wsp-test-buffer-text buffer)))
      (should (string-match-p "number 06" (wsp-test-buffer-text buffer)))
      (should (string-match-p "older messages" (wsp-test-buffer-text buffer)))
      (with-current-buffer buffer (wsp-chat-load-more))
      (should (string-match-p "number 02" (wsp-test-buffer-text buffer)))
      (should-not (string-match-p "number 01" (wsp-test-buffer-text buffer)))
      (with-current-buffer buffer (wsp-chat-load-more))
      (should (string-match-p "number 00" (wsp-test-buffer-text buffer)))
      (should-not (string-match-p "older messages" (wsp-test-buffer-text buffer))))))

(ert-deftest wsp-test-redraw-keeps-boundary-point-and-window-starts ()
  (wsp-test-with-store
    (wsp-test-chat wsp-test-bob)
    (dotimes (i 30)
      (wsp-test-message wsp-test-bob (format "M%02d" i) (* 10 (1+ i))
                        :sender wsp-test-bob :text (format "message number %02d" i)))
    (let* ((wsp-history-page-size 10)
           (wsp-message-grouping-interval 0)
           (buffer (wsp-chat-buffer wsp-test-bob)))
      (with-current-buffer buffer (wsp-chat-load-more))
      (delete-other-windows)
      (switch-to-buffer buffer)
      (let ((other (split-window)))
        (set-window-buffer other buffer)
        (with-current-buffer buffer
          (let ((target (wsp-chat--message-start "M12")))
            (set-window-point (selected-window) target)
            (set-window-start other (wsp-chat--message-start "M15")))
          (goto-char (wsp-chat--message-start "M12"))
          (goto-char (point-max))
          (insert "draft text"))
        (dotimes (i 15)
          (wsp-test-message wsp-test-bob (format "N%02d" i) (+ 1000 i)
                            :sender wsp-test-bob :text (format "new arrival %02d" i)))
        (wsp-chat-redraw buffer)
        (with-current-buffer buffer
          (should (string-match-p "number 10" (wsp-test-buffer-text buffer)))
          (should (equal (get-text-property (window-start other) 'wsp-id) "M15"))
          (should (string-suffix-p "draft text" (wsp-test-buffer-text buffer))))
        (delete-other-windows)))))

;;;; Opening at a message

(ert-deftest wsp-test-message-buffer-plain-ref ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-message-buffer (concat wsp-test-alice "/A3"))))
      (with-current-buffer buffer
        (should (equal (get-text-property (point) 'wsp-id) "A3"))
        (should wsp-chat--target-overlay)))))

(ert-deftest wsp-test-message-buffer-alias-ref ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-message-buffer (concat wsp-test-alice-lid "/A1"))))
      (should (eq buffer (wsp-chat-buffer wsp-test-alice)))
      (with-current-buffer buffer
        (should (equal (get-text-property (point) 'wsp-id) "A1"))))))

(ert-deftest wsp-test-message-buffer-ref-with-stale-unrecorded-chat ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-message-buffer "99999999999@lid/B1")))
      (should (eq buffer (wsp-chat-buffer wsp-test-bob))))))

(ert-deftest wsp-test-message-buffer-loads-old-history ()
  (wsp-test-with-store
    (wsp-test-chat wsp-test-bob)
    (dotimes (i 50)
      (wsp-test-message wsp-test-bob (format "M%02d" i) (* 10 (1+ i))
                        :sender wsp-test-bob :text (format "message number %02d" i)))
    (let* ((wsp-history-page-size 5)
           (buffer (wsp-message-buffer (concat wsp-test-bob "/M03"))))
      (with-current-buffer buffer
        (should (equal (get-text-property (point) 'wsp-id) "M03"))
        (should (string-match-p "number 49" (wsp-test-buffer-text buffer)))))))

(ert-deftest wsp-test-message-buffer-deleted-message-is-found ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-message-buffer (concat wsp-test-alice "/A4"))
      (should (equal (get-text-property (point) 'wsp-id) "A4"))
      (should (looking-at-p "  \\[deleted\\]")))))

(ert-deftest wsp-test-message-buffer-missing-message-in-known-chat ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((err (should-error (wsp-message-buffer (concat wsp-test-alice "/GONE"))
                             :type 'wsp-message-not-found)))
      (should (eq (nth 2 err) (wsp-chat-buffer wsp-test-alice))))))

(ert-deftest wsp-test-open-message-missing-falls-back-to-chat ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-open-message (concat wsp-test-alice "/GONE"))))
      (should (eq buffer (wsp-chat-buffer wsp-test-alice)))
      (should (eq (window-buffer (selected-window)) buffer)))))

(ert-deftest wsp-test-open-message-unknown-chat-signals ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((err (should-error (wsp-message-buffer "15559999999@s.whatsapp.net/X")
                             :type 'wsp-message-not-found)))
      (should-not (nth 2 err)))
    (should-error (wsp-open-message "15559999999@s.whatsapp.net/X")
                  :type 'wsp-message-not-found)
    (should-error (wsp-open-message "no-slash") :type 'user-error)))

;;;; Media and health

(ert-deftest wsp-test-media-found-unnamed-and-under-alias ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-message wsp-test-alice "A6" 1001 :sender wsp-test-alice :media "image")
    (let ((alias-dir (expand-file-name (wsp-media-safe-name wsp-test-alice-lid) (wsp-media-directory)))
          (dir (expand-file-name (wsp-media-safe-name wsp-test-alice) (wsp-media-directory))))
      (make-directory alias-dir t)
      (make-directory dir t)
      (with-temp-file (expand-file-name "A5-cat.jpg" alias-dir) (insert "x"))
      (with-temp-file (expand-file-name "A6-image.jpeg" dir) (insert "x"))
      (should (equal (wsp-media-file wsp-test-alice "A5") (expand-file-name "A5-cat.jpg" alias-dir)))
      (should (equal (wsp-media-file wsp-test-alice "A6") (expand-file-name "A6-image.jpeg" dir)))
      (should-not (wsp-media-file wsp-test-alice "A1")))))

(ert-deftest wsp-test-media-safe-name-matches-wa-archive ()
  (should (equal (wsp-media-safe-name "15550000001@s.whatsapp.net") "15550000001_s.whatsapp.net"))
  (should (equal (wsp-media-safe-name "a b-c_d.e/f:g") "a b-c_d.e_f_g")))

(ert-deftest wsp-test-health-warnings-and-file-only-changes ()
  (wsp-test-with-store
    (should (string-match-p "no heartbeat" (wsp-db-health)))
    (let ((before (wsp-db-health-signature)))
      (with-temp-file (expand-file-name "heartbeat.json" wsp-state-directory)
        (insert (format "{\"event\":\"connected\",\"at\":\"%s\"}"
                        (format-time-string "%FT%TZ" nil t))))
      (should-not (wsp-db-health))
      (should-not (equal before (wsp-db-health-signature))))
    (with-temp-file (expand-file-name "heartbeat.json" wsp-state-directory)
      (insert (format "{\"event\":\"connected\",\"at\":\"%s\"}"
                      (format-time-string "%FT%TZ" (time-subtract nil 7200) t))))
    (should (string-match-p "120 minutes old" (wsp-db-health)))
    (with-temp-file (expand-file-name "unlinked" wsp-state-directory))
    (should (string-match-p "unlinked" (wsp-db-health)))
    (with-temp-file (expand-file-name "outdated" wsp-state-directory))
    (should (string-match-p "outdated" (wsp-db-health)))))

(ert-deftest wsp-test-refresh-notices-media-download ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-chat-buffer wsp-test-alice))
          (dir (expand-file-name (wsp-media-safe-name wsp-test-alice) (wsp-media-directory))))
      (should-not (wsp-chat-media-changed-p buffer))
      (make-directory dir t)
      (with-temp-file (expand-file-name "A5-cat.jpg" dir) (insert "x"))
      (should (wsp-chat-media-changed-p buffer)))))

;;;; Search

(ert-deftest wsp-test-search ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-message wsp-test-bob "B2" 950 :sender wsp-test-bob :text "50% off_now")
    (should (equal (mapcar (lambda (m) (plist-get m :id)) (wsp-db-search "hello"))
                   '("G1" "A1")))
    (should (equal (mapcar (lambda (m) (plist-get m :id)) (wsp-db-search "hello" wsp-test-alice))
                   '("A1")))
    (should (equal (mapcar (lambda (m) (plist-get m :id)) (wsp-db-search "fixed")) '("A3")))
    (should-not (wsp-db-search "secret"))
    (should (equal (mapcar (lambda (m) (plist-get m :id)) (wsp-db-search "% off_")) '("B2")))
    (should-not (wsp-db-search "%%"))
    (should (equal (mapcar (lambda (m) (plist-get m :id)) (wsp-db-search "cat.jpg")) '("A5")))))

(ert-deftest wsp-test-search-result-opens-message ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-search "bob")
    (with-current-buffer wsp-search-buffer-name
      (wsp-search-goto-result))
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (should (equal (get-text-property (point) 'wsp-id) "B1")))))

;;;; Acting through the API

(ert-deftest wsp-test-send-success ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (let ((buffer (wsp-chat-buffer wsp-test-bob)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "a new message")
        (wsp-chat-send-input)
        (should (equal (wsp-chat--input-text) ""))
        (should (wsp-test-wait (lambda () (null wsp-chat--pending)))))
      (let ((request (car wsp-test-requests)))
        (should (equal (plist-get request :method) "send"))
        (should (equal (plist-get (plist-get request :params) :text) "a new message"))
        (should (equal (plist-get (plist-get request :params) :chat) wsp-test-bob))
        (should (plist-get (plist-get request :params) :op))))))

(ert-deftest wsp-test-send-failed-restores-input ()
  (wsp-test-with-store
    (wsp-test-populate)
    (setq wsp-test-handler (lambda (_m _p) '(:error "failed")))
    (wsp-test-connect)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "will fail")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (wsp-chat--input-text) "will fail")))))

(ert-deftest wsp-test-send-uncertain-is-kept-and-never-resent ()
  (wsp-test-with-store
    (wsp-test-populate)
    (setq wsp-test-handler
          (lambda (method _p)
            (pcase method
              ("send" '(:error "uncertain"))
              ("op_status" '(:result (:state "pending"))))))
    (wsp-test-connect)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "maybe sent")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (member "op_status" (wsp-test-methods)))))
      (accept-process-output nil 0.2)
      (should (equal (plist-get (car wsp-chat--pending) :state) 'uncertain))
      (should (equal (wsp-chat--input-text) ""))
      (should (string-match-p "may have been sent.*maybe sent" (wsp-test-buffer-text (current-buffer))))
      (should (equal (cl-count "send" (wsp-test-methods) :test #'equal) 1))
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'wsp-pending))
      (wsp-chat-dismiss-pending)
      (should-not wsp-chat--pending))))

(ert-deftest wsp-test-lost-answer-is-uncertain-then-resolved ()
  (wsp-test-with-store
    (wsp-test-populate)
    (setq wsp-test-handler
          (lambda (method _p)
            (pcase method
              ("send" :none)
              ("op_status" '(:result (:state "sent" :message_id "NEW1"))))))
    (let ((wsp-api-timeout 1))
      (wsp-test-connect)
      (with-current-buffer (wsp-chat-buffer wsp-test-bob)
        (goto-char (point-max))
        (insert "answer lost")
        (wsp-chat-send-input)
        (should (wsp-test-wait (lambda () (member "op_status" (wsp-test-methods))) 5))
        (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
        (should (equal (cl-count "send" (wsp-test-methods) :test #'equal) 1))))))

(ert-deftest wsp-test-reply-edit-react-delete-requests ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (with-current-buffer (wsp-chat-buffer wsp-test-alice)
      (goto-char (wsp-chat--message-start "A1"))
      (wsp-chat-reply)
      (insert "a reply")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (plist-get (plist-get (car wsp-test-requests) :params) :reply_to) "A1"))
      (goto-char (wsp-chat--message-start "A2"))
      (wsp-chat-edit)
      (should (equal (wsp-chat--input-text) "hi Alice"))
      (wsp-chat--set-input "hi Alice!")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (plist-get (car wsp-test-requests) :method) "edit"))
      (should (equal (plist-get (plist-get (car wsp-test-requests) :params) :id) "A2"))
      (goto-char (wsp-chat--message-start "A1"))
      (should-error (wsp-chat-edit) :type 'user-error)
      (wsp-chat-react "❤️")
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (plist-get (plist-get (car wsp-test-requests) :params) :emoji) "❤️"))
      (goto-char (wsp-chat--message-start "A2"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (wsp-chat-delete))
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (plist-get (car wsp-test-requests) :method) "delete")))))

(ert-deftest wsp-test-acting-without-daemon-is-refused ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "nowhere to go")
      (should-error (wsp-chat-send-input) :type 'user-error)
      (should-not wsp-chat--pending))))

(ert-deftest wsp-test-mark-read-when-shown ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (let ((wsp-mark-read t))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
        (let ((buffer (wsp-chat-buffer wsp-test-bob)))
          (switch-to-buffer buffer)
          (wsp-chat--maybe-mark-read buffer)
          (should (wsp-test-wait (lambda () (member "mark_read" (wsp-test-methods)))))
          (should (equal (plist-get (plist-get (car wsp-test-requests) :params) :chat)
                         wsp-test-bob)))))))

(ert-deftest wsp-test-no-mark-read-when-not-shown-or-off ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
      (let ((buffer (wsp-chat-buffer wsp-test-bob)))
        (let ((wsp-mark-read t))
          (switch-to-buffer "*scratch*")
          (wsp-chat--maybe-mark-read buffer))
        (let ((wsp-mark-read nil))
          (switch-to-buffer buffer)
          (wsp-chat--maybe-mark-read buffer))
        (accept-process-output nil 0.2)
        (should-not (member "mark_read" (wsp-test-methods)))))))

(ert-deftest wsp-test-no-presence-or-typing-by-default ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-start-server)
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
      (wsp)
      (wsp-test-wait #'wsp-api-connected-p)
      (let ((buffer (wsp-chat-open wsp-test-bob)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "typing away")))
      (wsp--on-focus)
      (wsp--on-selection)
      (accept-process-output nil 0.3)
      (should-not (seq-intersection (wsp-test-methods) '("presence" "typing"))))))

(ert-deftest wsp-test-presence-and-typing-when-enabled ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-start-server)
    (let ((wsp-send-presence t)
          (wsp-send-typing t))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
        (wsp)
        (wsp-test-wait #'wsp-api-connected-p)
        (with-current-buffer (wsp-chat-open wsp-test-bob)
          (goto-char (point-max))
          (insert "typing away"))
        (wsp--on-focus)
        (should (wsp-test-wait (lambda () (and (member "presence" (wsp-test-methods))
                                               (member "typing" (wsp-test-methods))))))))))

(ert-deftest wsp-test-changed-event-redraws ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-start-server)
    (wsp)
    (wsp-test-wait #'wsp-api-connected-p)
    (let ((buffer (wsp-chat-buffer wsp-test-bob)))
      (wsp-test-message wsp-test-bob "B9" 2000 :sender wsp-test-bob :text "fresh arrival")
      (wsp-test-push-event (list :event "changed" :chats (vector wsp-test-bob)))
      (should (wsp-test-wait (lambda () (string-match-p "fresh arrival"
                                                        (wsp-test-buffer-text buffer))))))))

;;;; Notifications, drafts, parsing

(ert-deftest wsp-test-notify-only-new-live-incoming ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((wsp-notify t) shown)
      (cl-letf (((symbol-function 'wsp-notify--desktop)
                 (lambda (title body) (push (cons title body) shown))))
        (wsp-notify-check)
        (should-not shown)
        (let ((now (floor (float-time))))
          (wsp-test-message wsp-test-bob "B3" now :sender wsp-test-bob :text "ping")
          (wsp-test-message wsp-test-bob "B4" (1+ now) :from-me t :text "pong" :source "sent")
          (wsp-test-message wsp-test-bob "B5" 10 :sender wsp-test-bob :text "old" :source "history")
          ;; An alias merge copies old live rows under new rowids.
          (wsp-test-message wsp-test-bob "B6" 20 :sender wsp-test-bob :text "merged"))
        (wsp-notify-check)
        (should (equal shown '(("Bobby" . "ping"))))))))

(ert-deftest wsp-test-drafts-survive-killing-the-buffer ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "unfinished thought"))
    (kill-buffer (wsp-chat-buffer wsp-test-bob))
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (should (equal (wsp-chat--input-text) "unfinished thought")))))

(ert-deftest wsp-test-parse-ref-and-number ()
  (should (equal (wsp--parse-ref "123@g.us/ABC") '("123@g.us" . "ABC")))
  (should-error (wsp--parse-ref "/ABC") :type 'user-error)
  (should-error (wsp--parse-ref "123@g.us/") :type 'user-error)
  (should (equal (wsp--number-key "+1 555 000-0001") "15550000001@s.whatsapp.net"))
  (should-not (wsp--number-key "Alice")))

;;;; Regressions from the code audit

(ert-deftest wsp-test-failed-redraw-keeps-input ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((buffer (wsp-chat-buffer wsp-test-bob)) (fail t))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "precious words"))
      (cl-letf* ((orig (symbol-function 'wsp-db-messages))
                 ((symbol-function 'wsp-db-messages)
                  (lambda (&rest args)
                    (if fail (progn (setq fail nil) (error "Simulated store error"))
                      (apply orig args)))))
        (should-error (wsp-chat-redraw buffer))
        (should (string-suffix-p "precious words" (wsp-test-buffer-text buffer)))
        (wsp-chat-redraw buffer)
        (with-current-buffer buffer
          (should (equal (wsp-chat--input-text) "precious words")))))))

(ert-deftest wsp-test-failed-edit-keeps-its-target ()
  (wsp-test-with-store
    (wsp-test-populate)
    (setq wsp-test-handler (lambda (_m _p) '(:error "failed")))
    (wsp-test-connect)
    (with-current-buffer (wsp-chat-buffer wsp-test-alice)
      (goto-char (wsp-chat--message-start "A2"))
      (wsp-chat-edit)
      (wsp-chat--set-input "hi Alice!!")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (wsp-chat--input-text) "hi Alice!!"))
      (should (equal (plist-get wsp-chat--edit-target :id) "A2"))
      (wsp-chat--set-input "")
      (setq wsp-chat--edit-target nil)
      (goto-char (wsp-chat--message-start "A1"))
      (wsp-chat-reply)
      (insert "a reply")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (plist-get wsp-chat--reply-to :id) "A1")))))

(ert-deftest wsp-test-uncertain-then-failed-returns-text ()
  (wsp-test-with-store
    (wsp-test-populate)
    (setq wsp-test-handler
          (lambda (method _p)
            (pcase method
              ("send" '(:error "uncertain"))
              ("op_status" '(:result (:state "failed" :error "no"))))))
    (wsp-test-connect)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "try me")
      (wsp-chat-send-input)
      (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
      (should (equal (wsp-chat--input-text) "try me")))))

(ert-deftest wsp-test-late-answer-settles-and-reconnect-rechecks ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let ((answer nil))
      (setq wsp-test-handler
            (lambda (method _p)
              (pcase method
                ("send" :none)
                ("op_status" (or answer '(:result (:state "pending")))))))
      (let ((wsp-api-timeout 1))
        (wsp-test-connect)
        (with-current-buffer (wsp-chat-buffer wsp-test-bob)
          (goto-char (point-max))
          (insert "slow one")
          (wsp-chat-send-input)
          (should (wsp-test-wait (lambda () (member "op_status" (wsp-test-methods))) 5))
          (accept-process-output nil 0.2)
          (should (eq (plist-get (car wsp-chat--pending) :state) 'uncertain))
          (setq answer '(:result (:state "sent" :message_id "X")))
          (wsp-chat-recheck-uncertain)
          (should (wsp-test-wait (lambda () (null wsp-chat--pending))))
          (should (equal (cl-count "send" (wsp-test-methods) :test #'equal) 1)))))))

(ert-deftest wsp-test-mark-read-does-not-repeat-when-stuck ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (let ((wsp-mark-read t))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
        (let ((buffer (wsp-chat-buffer wsp-test-bob)))
          (switch-to-buffer buffer)
          (wsp-chat--maybe-mark-read buffer)
          (should (wsp-test-wait (lambda () (not (buffer-local-value 'wsp-chat--marking-read buffer)))))
          (wsp-chat--maybe-mark-read buffer)
          (accept-process-output nil 0.2)
          (should (equal (cl-count "mark_read" (wsp-test-methods) :test #'equal) 1)))))))

(ert-deftest wsp-test-no-mark-read-on-unknown-focus ()
  (wsp-test-with-store
    (wsp-test-populate)
    (wsp-test-connect)
    (let ((wsp-mark-read t))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) 'unknown)))
        (let ((buffer (wsp-chat-buffer wsp-test-bob)))
          (switch-to-buffer buffer)
          (wsp-chat--maybe-mark-read buffer)
          (accept-process-output nil 0.2)
          (should-not (member "mark_read" (wsp-test-methods))))))))

(ert-deftest wsp-test-drafts-saved-at-exit-and-corrupt-file-kept ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (goto-char (point-max))
      (insert "draft at exit"))
    (wsp-chat-save-all-drafts)
    (should (equal (cdr (assoc wsp-test-bob (wsp-chat--read-drafts))) "draft at exit"))
    (should (= (file-modes wsp-drafts-file) #o600))
    (with-temp-file wsp-drafts-file (insert "(not a draft list"))
    (with-current-buffer (wsp-chat-buffer wsp-test-bob)
      (wsp-chat--save-draft))
    (should (equal (with-temp-buffer (insert-file-contents wsp-drafts-file) (buffer-string))
                   "(not a draft list"))))

(ert-deftest wsp-test-unsafe-attachment-asks-first ()
  (wsp-test-with-store
    (wsp-test-populate)
    (let (opened asked)
      (cl-letf (((symbol-function 'wsp-media-open) (lambda (f) (push f opened)))
                ((symbol-function 'yes-or-no-p) (lambda (_) (setq asked t) nil)))
        (wsp-actions--open-file '(:media-type "document") "/x/42-run.terminal")
        (should asked)
        (should-not opened)
        (setq asked nil)
        (wsp-actions--open-file '(:media-type "document") "/x/43-notes.pdf")
        (should-not asked)
        (should (equal opened '("/x/43-notes.pdf")))))))

(ert-deftest wsp-test-edit-asks-before-replacing-input ()
  (wsp-test-with-store
    (wsp-test-populate)
    (with-current-buffer (wsp-chat-buffer wsp-test-alice)
      (goto-char (point-max))
      (insert "keep me")
      (goto-char (wsp-chat--message-start "A2"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (should-error (wsp-chat-edit) :type 'user-error))
      (should (equal (wsp-chat--input-text) "keep me"))
      (should-not wsp-chat--edit-target))))

(ert-deftest wsp-test-notice-lines-are-read-only ()
  (wsp-test-with-store
    (wsp-test-chat wsp-test-carol :name "Carol")
    (with-current-buffer (wsp-chat-buffer wsp-test-carol)
      (goto-char (point-min))
      (should (get-text-property (point) 'read-only)))))

(provide 'wsp-test)
;;; wsp-test.el ends here
