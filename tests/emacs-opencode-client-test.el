;;; emacs-opencode-client-test.el --- Tests for HTTP client  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-opencode-client)

(defmacro opencode-client-test--with-captured-request (url-var args-var &rest body)
  "Run BODY with `request' stubbed to capture its arguments.
URL-VAR and ARGS-VAR are bound to the URL and keyword arguments of the
last `request' call made during BODY."
  (declare (indent 2))
  `(let ((,url-var nil)
         (,args-var nil))
     (cl-letf (((symbol-function 'request)
                (lambda (&rest request-args)
                  (setq ,url-var (car request-args)
                        ,args-var (cdr request-args))
                  nil)))
       ,@body)))

(defun opencode-client-test--connection (&optional directory)
  "Return a connection for DIRECTORY pointed at a fake base URL."
  (opencode-connection-create
   :base-url "http://127.0.0.1:4096"
   :hostname "127.0.0.1"
   :port 4096
   :directory directory
   :timeout 10))

;;; directory header

(ert-deftest test-opencode-client/request-sends-directory-header ()
  "Every request includes an url-encoded x-opencode-directory header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-request conn 'GET "/agent")
      (should (equal url "http://127.0.0.1:4096/agent"))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers))
                       (url-hexify-string "/tmp/project")))))))

(ert-deftest test-opencode-client/request-directory-header-strips-trailing-slash ()
  "Directory header value has no trailing slash."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent")
      (let* ((headers (plist-get args :headers))
             (value (cdr (assoc "x-opencode-directory" headers))))
        (should-not (string-suffix-p "%2F" value))
        (should-not (string-suffix-p "/" value))))))

(ert-deftest test-opencode-client/request-no-directory-header-without-directory ()
  "No directory header when the connection has no directory."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent")
      (should-not (assoc "x-opencode-directory" (plist-get args :headers))))))

(ert-deftest test-opencode-client/request-directory-header-with-json ()
  "Directory header coexists with the JSON content type header."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'POST "/session/s1/abort" :json '((a . 1)))
      (let ((headers (plist-get args :headers)))
        (should (assoc "x-opencode-directory" headers))
        (should (equal (cdr (assoc "Content-Type" headers))
                       "application/json"))))))

(ert-deftest test-opencode-client/request-respects-caller-directory-header ()
  "An explicit x-opencode-directory header is not overridden."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-request conn 'GET "/agent"
                        :headers '(("x-opencode-directory" . "custom")))
      (let ((headers (plist-get args :headers)))
        (should (equal (cdr (assoc "x-opencode-directory" headers)) "custom"))
        (should (= 1 (cl-count "x-opencode-directory" headers
                               :key #'car :test #'equal)))))))

;;; session-create

(ert-deftest test-opencode-client/session-create-sends-location ()
  "Session creation posts /api/session with the directory as location."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data)
                     (json-encode '((location
                                     . ((directory . "/tmp/project"))))))))))

(ert-deftest test-opencode-client/session-create-without-directory-sends-empty-object ()
  "Session creation without a directory posts an empty JSON object."
  (let ((conn (opencode-client-test--connection nil)))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-create conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :data) "{}")))))

;;; sessions

(ert-deftest test-opencode-client/sessions-no-limit-omits-params ()
  "Listing sessions without a limit sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-sessions conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 1000))))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/sessions-forwards-roots ()
  "Root-only session lists request server-side filtering."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-sessions conn :limit 1000 :roots t
                                :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params)
                     '(("roots" . "true") ("limit" . 1000)))))))

;;; session-messages

(ert-deftest test-opencode-client/session-messages-no-limit-omits-params ()
  "Fetching messages without a limit sends no query params."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-messages conn "ses_1"
                                         :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/message"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :params)))
      (should (null (plist-get args :data))))))

(ert-deftest test-opencode-client/session-messages-forwards-limit ()
  "A limit is forwarded as a limit query parameter, not a request body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-messages conn "ses_1" :limit 5
                                         :success #'ignore :error #'ignore)
      (should (equal (plist-get args :params) '(("limit" . 5))))
      (should (null (plist-get args :data))))))

;;; session-message

(ert-deftest test-opencode-client/session-message-fetches-single-message ()
  "Fetching one message hits the message endpoint with both IDs."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-message conn "ses_1" "msg_1"
                                        :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/message/msg_1"))
      (should (equal (plist-get args :type) "GET"))
      (should (null (plist-get args :data))))))

;;; session-fork

(ert-deftest test-opencode-client/session-fork-without-message-sends-empty-object ()
  "Forking a whole session posts an empty JSON object."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-fork conn "ses_1" :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/fork"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data) "{}")))))

(ert-deftest test-opencode-client/session-fork-with-message-sends-before ()
  "Forking at a message sends MESSAGE-ID as before."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-fork conn "ses_1" :message-id "msg_1"
                                    :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((before . "msg_1"))))))))

;;; session-rename

(ert-deftest test-opencode-client/session-rename-sends-title ()
  "Renaming a session patches its title."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-rename conn "ses_1" "New title"
                                      :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1"))
      (should (equal (plist-get args :type) "PATCH"))
      (should (equal (plist-get args :data)
                     (json-encode '((title . "New title"))))))))

;;; session-compact

(ert-deftest test-opencode-client/session-compact-sends-empty-object ()
  "Compacting a session posts an empty JSON object."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-compact conn "ses_1"
                                       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/compact"))
      (should (equal (plist-get args :type) "POST"))
      (should (equal (plist-get args :data) "{}")))))

;;; session-abort

(ert-deftest test-opencode-client/session-abort-posts-interrupt ()
  "Aborting posts to the interrupt endpoint with no body."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-abort conn "ses_1"
                                     :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/interrupt"))
      (should (equal (plist-get args :type) "POST"))
      (should (null (plist-get args :data))))))

;;; session-command

(ert-deftest test-opencode-client/session-command-sends-name-and-text ()
  "Commands post name and text arguments."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-command conn "ses_1" "commit" "all"
                                       :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/command"))
      (should (equal (plist-get args :data)
                     (json-encode '((name . "commit")
                                    (text . "all"))))))))

(ert-deftest test-opencode-client/session-command-sends-agent ()
  "A command agent is sent as an agent attachment."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request _url args
      (opencode-client-session-command conn "ses_1" "commit" ""
                                       :agent "plan"
                                       :success #'ignore :error #'ignore)
      (should (equal (plist-get args :data)
                     (json-encode '((name . "commit")
                                    (text . "")
                                    (agents . (((name . "plan")))))))))))

;;; session-shell

(ert-deftest test-opencode-client/session-shell-sends-command ()
  "Shell execution posts only the command."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-session-shell conn "ses_1" "ls"
                                     :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/session/ses_1/shell"))
      (should (equal (plist-get args :data)
                     (json-encode '((command . "ls"))))))))

;;; form-reply

(ert-deftest test-opencode-client/form-reply-sends-answer ()
  "Form replies post the answer map."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-reply conn "ses_1" "frm_1" '((color . "red"))
                                  :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/session/ses_1/form/frm_1/reply")))
      (should (equal (plist-get args :data)
                     (json-encode '((answer . ((color . "red"))))))))))

(ert-deftest test-opencode-client/form-cancel-deletes-form ()
  "Form cancellation deletes the form."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-form-cancel conn "ses_1" "frm_1"
                                   :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/session/ses_1/form/frm_1")))
      (should (equal (plist-get args :type) "DELETE")))))

;;; integration-connect

(ert-deftest test-opencode-client/integrations-list ()
  "Integration listing hits the v2 endpoint."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integrations conn :success #'ignore :error #'ignore)
      (should (equal url "http://127.0.0.1:4096/api/integration"))
      (should (equal (plist-get args :type) "GET")))))

(ert-deftest test-opencode-client/integration-connect-key ()
  "Key connect posts the key."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-connect-key conn "anthropic" "sk-x"
                                               :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/integration/anthropic/connect/key")))
      (should (equal (plist-get args :data)
                     (json-encode '((key . "sk-x"))))))))

(ert-deftest test-opencode-client/integration-oauth-begin ()
  "OAuth begin posts the method ID."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-oauth-begin conn "openai" "chatgpt-browser"
                                               :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/integration/openai/connect/oauth")))
      (should (equal (plist-get args :data)
                     (json-encode '((methodID . "chatgpt-browser"))))))))

(ert-deftest test-opencode-client/integration-oauth-status ()
  "OAuth status polls the attempt."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-oauth-status conn "openai" "con_1"
                                                :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/integration/openai/connect/oauth/con_1")))
      (should (equal (plist-get args :type) "GET")))))

(ert-deftest test-opencode-client/integration-oauth-complete ()
  "OAuth complete posts the code."
  (let ((conn (opencode-client-test--connection "/tmp/project/")))
    (opencode-client-test--with-captured-request url args
      (opencode-client-integration-oauth-complete conn "openai" "con_1"
                                                  :code "abc"
                                                  :success #'ignore :error #'ignore)
      (should (equal url (concat "http://127.0.0.1:4096"
                                 "/api/integration/openai/connect/oauth/con_1/complete")))
      (should (equal (plist-get args :data)
                     (json-encode '((code . "abc"))))))))

;;; format-error

(ert-deftest test-opencode-client/format-error-status-and-tag ()
  "Format status code and server error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response :data '((_tag . "BadRequest"))))
                   "HTTP 400: BadRequest"))))

(ert-deftest test-opencode-client/format-error-prefers-message-field ()
  "Prefer a server-provided message over the error tag."
  (let ((response (make-request-response :status-code 400)))
    (should (equal (opencode-client-format-error
                    (list :response response
                          :data '((_tag . "BadRequest")
                                  (message . "directory is invalid"))))
                   "HTTP 400: directory is invalid"))))

(ert-deftest test-opencode-client/format-error-status-only ()
  "Fall back to the status code alone."
  (let ((response (make-request-response :status-code 500)))
    (should (equal (opencode-client-format-error (list :response response))
                   "HTTP 500"))))

(ert-deftest test-opencode-client/format-error-error-thrown ()
  "Fall back to the thrown error when there is no response."
  (should (equal (opencode-client-format-error
                  (list :error-thrown '(error . "connection refused")))
                 "(error . connection refused)")))

(ert-deftest test-opencode-client/format-error-nil-without-detail ()
  "Return nil when no detail is available."
  (should (null (opencode-client-format-error nil))))

(provide 'emacs-opencode-client-test)

;;; emacs-opencode-client-test.el ends here
