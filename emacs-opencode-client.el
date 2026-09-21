;;; emacs-opencode-client.el --- OpenCode HTTP client  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'subr-x)
(require 'url-util)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-sse)

(declare-function opencode--json-read "emacs-opencode-sse")

(defun opencode-client--directory-header (conn headers)
  "Return a directory routing header alist for CONN.

Returns nil when CONN has no directory or HEADERS already contains an
x-opencode-directory entry.  The header value is the connection directory
without its trailing slash, url-encoded to match the OpenCode SDK."
  (when-let* ((directory (opencode-connection-directory conn)))
    (unless (assoc "x-opencode-directory" headers)
      `(("x-opencode-directory"
         . ,(url-hexify-string (directory-file-name directory)))))))

(defun opencode-client-format-error (args)
  "Return a short description of a failed request from ARGS.

ARGS is the keyword plist passed to `request' error callbacks.  Returns a
string like \"HTTP 400: BadRequest\", or nil when no detail is available."
  (let* ((response (plist-get args :response))
         (status (and response (request-response-status-code response)))
         (data (plist-get args :data))
         (detail (and (listp data)
                      (or (alist-get 'message data)
                          (alist-get '_tag data))))
         (error-thrown (plist-get args :error-thrown)))
    (cond
     ((and status detail) (format "HTTP %s: %s" status detail))
     (status (format "HTTP %s" status))
     (detail (format "%s" detail))
     (error-thrown (format "%s" error-thrown)))))

(cl-defmethod opencode-request ((conn opencode-connection) method path &rest args &key data json parser headers timeout &allow-other-keys)
  "Send a raw HTTP request using CONN.

METHOD is a HTTP verb symbol like `GET` or `POST`. PATH is appended to the
connection base URL. DATA is passed through to `request`. When JSON is
provided, it is encoded and sent with a JSON content type. PARSER defaults to
`json-read` when omitted. HEADERS is an alist of HTTP headers. Any remaining
ARGS are forwarded to `request`.  Every request carries an
x-opencode-directory header derived from the connection directory so the
server routes session-less requests to the right workspace.  When the
connection has a password, an explicit `Authorization: Basic ...' header is
added (unless the caller already supplied one)."
  (let* ((base-url (opencode-connection-base-url conn))
         (url (concat (string-remove-suffix "/" base-url) path))
         (auth-header (when-let* ((password (opencode-connection-password conn)))
                        (unless (assoc-string "Authorization" headers t)
                          (let ((user (or (opencode-connection-username conn)
                                          "opencode")))
                            `(("Authorization"
                               . ,(concat "Basic "
                                          (base64-encode-string
                                           (format "%s:%s" user password)
                                           t))))))))
         (payload (when json (json-encode json)))
         (merged-headers (append (opencode-client--directory-header conn headers)
                                 auth-header
                                 headers
                                 (when json
                                   '(("Content-Type" . "application/json")))))
         (timeout-value (if (plist-member args :timeout)
                            timeout
                          (or (opencode-connection-timeout conn) 10))))
    (apply
     #'request
     url
     :type (symbol-name method)
     :data (or payload data)
     :parser (or parser #'opencode--json-read)
     :headers merged-headers
     :timeout timeout-value
     args)))

(cl-defmethod opencode-client-health ((conn opencode-connection) &key success error)
  "Fetch OpenCode server health."
  (opencode-request
   conn
   'GET
   "/api/info"
   :success success
   :error error))

(cl-defmethod opencode-client-session-create ((conn opencode-connection) &key success error)
  "Create a new session in CONN's directory.

The server resolves the session location from the `location' object in
the request body."
  (opencode-request
   conn
   'POST
   "/api/session"
   :json (if-let* ((directory (opencode-connection-directory conn)))
             `((location . ((directory . ,(directory-file-name directory)))))
           (make-hash-table :test 'equal))
   :success success
   :error error))

(cl-defmethod opencode-client-sessions ((conn opencode-connection) &key success error limit roots)
  "Fetch OpenCode sessions list.

LIMIT restricts the number of returned sessions when provided.  The
server caps the list at 100 sessions by default, so a higher LIMIT is
required to retrieve more.  When ROOTS is non-nil, return only sessions
without a parent."
  (opencode-request
   conn
   'GET
   "/api/session"
   :params (append (when roots '(("roots" . "true")))
                   (when limit `(("limit" . ,limit))))
   :success success
   :error error))

(cl-defmethod opencode-client-session-messages ((conn opencode-connection) session-id &key success error limit)
  "Fetch messages for SESSION-ID.

LIMIT restricts the number of returned messages when provided."
  (opencode-request
   conn
   'GET
   (format "/api/session/%s/message" session-id)
   :params (when limit `(("limit" . ,limit)))
    :success success
    :error error))

(cl-defmethod opencode-client-session-message ((conn opencode-connection) session-id message-id &key success error)
  "Fetch the single message MESSAGE-ID in SESSION-ID.

Unlike the SSE stream, the response includes full tool output in each
part's state."
  (opencode-request
   conn
   'GET
   (format "/api/session/%s/message/%s" session-id message-id)
   :success success
   :error error))

(cl-defmethod opencode-client-session-fork ((conn opencode-connection) session-id &key message-id success error)
  "Fork SESSION-ID.

When MESSAGE-ID is provided, fork the session before that message.  When
MESSAGE-ID is nil, fork the whole session."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/fork" session-id)
   :json (if message-id
             `((before . ,message-id))
           (make-hash-table :test 'equal))
   :success success
   :error error))

(cl-defmethod opencode-client-session-rename
  ((conn opencode-connection) session-id title &key success error)
  "Use CONN to rename SESSION-ID to TITLE."
  (opencode-request
   conn
   'PATCH
   (format "/api/session/%s" session-id)
   :json `((title . ,title))
   :success success
   :error error))

(cl-defmethod opencode-client-session-compact
  ((conn opencode-connection) session-id &key success error)
  "Compact SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/compact" session-id)
   :json (make-hash-table :test 'equal)
   :parser (lambda () nil)
   :timeout nil
   :success success
   :error error))

(cl-defmethod opencode-client-agents ((conn opencode-connection) &key success error)
  "Fetch available agents from the server."
  (opencode-request
   conn
   'GET
   "/api/agent"
   :success success
   :error error))

(cl-defmethod opencode-client-providers ((conn opencode-connection) &key success error)
  "Fetch available providers from the server."
  (opencode-request
   conn
   'GET
   "/api/provider"
   :success success
   :error error))

(cl-defmethod opencode-client-models ((conn opencode-connection) &key success error)
  "Fetch available models from the server."
  (opencode-request
   conn
   'GET
   "/api/model"
   :success success
   :error error))

(cl-defmethod opencode-client-commands ((conn opencode-connection) &key success error)
  "Fetch available commands from the server."
  (opencode-request
   conn
   'GET
   "/api/command"
   :success success
   :error error))

(cl-defmethod opencode-client-integrations ((conn opencode-connection) &key success error)
  "Fetch integrations and their auth methods."
  (opencode-request
   conn
   'GET
   "/api/integration"
   :success success
   :error error))

(cl-defmethod opencode-client-integration-connect-key
  ((conn opencode-connection) integration-id key &key answer success error)
  "Connect INTEGRATION-ID with KEY.
ANSWER is an optional alist of extra form values."
  (let ((payload `((key . ,key))))
    (when answer
      (setq payload (append payload `((answer . ,answer)))))
    (opencode-request
     conn
     'POST
     (format "/api/integration/%s/connect/key" integration-id)
     :json payload
     :parser (lambda () nil)
     :success success
     :error error)))

(cl-defmethod opencode-client-integration-oauth-begin
  ((conn opencode-connection) integration-id method-id &key answer success error)
  "Begin an OAuth attempt for INTEGRATION-ID using METHOD-ID.
ANSWER is an optional alist of extra form values."
  (let ((payload `((methodID . ,method-id))))
    (when answer
      (setq payload (append payload `((answer . ,answer)))))
    (opencode-request
     conn
     'POST
     (format "/api/integration/%s/connect/oauth" integration-id)
     :json payload
     :success success
     :error error)))

(cl-defmethod opencode-client-integration-oauth-status
  ((conn opencode-connection) integration-id attempt-id &key success error)
  "Poll the OAuth attempt ATTEMPT-ID for INTEGRATION-ID."
  (opencode-request
   conn
   'GET
   (format "/api/integration/%s/connect/oauth/%s" integration-id attempt-id)
   :success success
   :error error))

(cl-defmethod opencode-client-integration-oauth-complete
  ((conn opencode-connection) integration-id attempt-id &key code success error)
  "Complete the OAuth attempt ATTEMPT-ID for INTEGRATION-ID.
CODE is the authorization code for \"code\" mode attempts."
  (opencode-request
   conn
   'POST
   (format "/api/integration/%s/connect/oauth/%s/complete"
           integration-id attempt-id)
   :json (if code
             `((code . ,code))
           (make-hash-table :test 'equal))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-prompt-async
  ((conn opencode-connection) session-id input &key success error agents)
  "Send input to SESSION-ID asynchronously.

AGENTs are agent names mentioned in the text."
  (opencode-request
    conn
    'POST
    (format "/api/session/%s/prompt" session-id)
    :json (append `((text . ,input))
                  (when agents `((agents . ,agents))))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-session-abort ((conn opencode-connection) session-id &key success error)
  "Abort the active prompt for SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/interrupt" session-id)
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-permission-reply
  ((conn opencode-connection) request-id reply session-id &key message success error)
  "Reply to permission REQUEST-ID with REPLY in SESSION-ID.

MESSAGE is sent when provided."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/permission/%s/reply" session-id request-id)
   :json `((decision . ,reply)
           ,@(when message `((message . ,message))))
   :success success
   :error error))

(cl-defmethod opencode-client-session-command
  ((conn opencode-connection) session-id command arguments
   &key agent success error)
  "Send COMMAND with ARGUMENTS to SESSION-ID.

AGENT names an agent attachment included when provided."
  (let ((payload `((name . ,command)
                   (text . ,(or arguments "")))))
    (when agent
      (setq payload (append payload (list (cons 'agents (list (list (cons 'name agent))))))))
    (opencode-request
     conn
     'POST
     (format "/api/session/%s/command" session-id)
     :json payload
     :parser (lambda () nil)
     :timeout nil
     :success success
     :error error)))

(cl-defmethod opencode-client-session-shell
  ((conn opencode-connection) session-id command
   &key success error)
  "Execute shell COMMAND in SESSION-ID."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/shell" session-id)
   :json `((command . ,command))
   :parser (lambda () nil)
   :success success
   :error error))

(cl-defmethod opencode-client-form-reply ((conn opencode-connection) session-id form-id answer &key success error)
  "Reply to form FORM-ID in SESSION-ID with ANSWER.

ANSWER is an alist mapping field keys to values."
  (opencode-request
   conn
   'POST
   (format "/api/session/%s/form/%s/reply" session-id form-id)
   :json `((answer . ,answer))
   :success success
   :error error))

(cl-defmethod opencode-client-form-cancel ((conn opencode-connection) session-id form-id &key success error)
  "Cancel form FORM-ID in SESSION-ID."
  (opencode-request
   conn
   'DELETE
   (format "/api/session/%s/form/%s" session-id form-id)
   :parser (lambda () nil)
   :success success
   :error error))

(provide 'emacs-opencode-client)

;;; emacs-opencode-client.el ends here
