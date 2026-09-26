;;; emacs-opencode-session-handlers.el --- SSE event handlers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-message)
(require 'emacs-opencode-client)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-session)
(require 'emacs-opencode-sse)

(declare-function opencode-session--update-session "emacs-opencode-session-mode")
(declare-function opencode-session--update-status "emacs-opencode-session-mode")
(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--upsert-message "emacs-opencode-session-mode")
(declare-function opencode-session--update-message-part "emacs-opencode-session-mode")
(declare-function opencode-session--find-message "emacs-opencode-session-mode")
(declare-function opencode-session--message-text "emacs-opencode-session-render")
(declare-function opencode-session--render-message "emacs-opencode-session-render")
(declare-function opencode-session--upsert-compaction "emacs-opencode-session-mode")
(declare-function opencode-session--ensure-stream-message "emacs-opencode-session-mode")
(declare-function opencode-session--upsert-stream-part "emacs-opencode-session-mode")
(declare-function opencode-session--stream-text-part-id "emacs-opencode-session-mode")
(declare-function opencode-session--register-subagent "emacs-opencode-session-mode")
(declare-function opencode-session--buffer-name "emacs-opencode-session-mode")
(declare-function opencode-session--rename-buffer "emacs-opencode-session-mode")
(declare-function opencode-session--set-server-model "emacs-opencode-session-model"
                  (ref))
(declare-function opencode-session--set-server-agent "emacs-opencode-session-model"
                  (agent))
(declare-function opencode-session--agents-changed "emacs-opencode-session-model"
                  (connection))

(cl-defstruct (opencode-session--prompt-state
               (:constructor opencode-session--prompt-state-create))
  connection
  request-id
  status)

(defvar opencode-session--pending-prompts (make-hash-table :test #'eq)
  "Pending permission and question prompts grouped by connection.")

(defvar opencode-session--active-prompt nil
  "State for the OpenCode prompt currently using the minibuffer.")

(defvar-local opencode-session--minibuffer-prompt nil
  "OpenCode prompt state associated with this minibuffer.")

(defun opencode-session--prompt-table (connection &optional create)
  "Return the pending prompt table for CONNECTION.
Create and register the table when CREATE is non-nil."
  (or (gethash connection opencode-session--pending-prompts)
      (when create
        (let ((table (make-hash-table :test #'equal)))
          (puthash connection table opencode-session--pending-prompts)
          table))))

(defun opencode-session--register-prompt (connection request-id)
  "Register REQUEST-ID as pending on CONNECTION.
Return its new state, or nil when it is already registered."
  (let ((table (opencode-session--prompt-table connection t)))
    (unless (gethash request-id table)
      (let ((state (opencode-session--prompt-state-create
                    :connection connection
                    :request-id request-id
                    :status 'pending)))
        (puthash request-id state table)
        state))))

(defun opencode-session--remove-prompt (state)
  "Remove pending prompt STATE from its connection registry."
  (let* ((connection (opencode-session--prompt-state-connection state))
         (request-id (opencode-session--prompt-state-request-id state))
         (table (opencode-session--prompt-table connection)))
    (when (and table (eq (gethash request-id table) state))
      (remhash request-id table)
      (when (zerop (hash-table-count table))
        (remhash connection opencode-session--pending-prompts)))))

(defun opencode-session--prompt-active-p (state)
  "Return non-nil when STATE owns the active minibuffer."
  (and (eq state opencode-session--active-prompt)
       (when-let* ((window (active-minibuffer-window)))
         (with-current-buffer (window-buffer window)
           (eq state opencode-session--minibuffer-prompt)))))

(defun opencode-session--run-prompt (state function)
  "Run prompt FUNCTION for STATE, cleaning up if it fails."
  (condition-case err
      (funcall function)
    (error
     (when (eq (opencode-session--prompt-state-status state) 'pending)
       (setf (opencode-session--prompt-state-status state) 'failed)
       (opencode-session--remove-prompt state))
     (signal (car err) (cdr err)))))

(defun opencode-session--handle-prompt-resolved (_event data meta)
  "Handle a prompt completion SSE DATA arriving with META."
  (let* ((properties (alist-get 'properties data))
         (request-id (or (alist-get 'requestID properties)
                         (alist-get 'id properties)))
         (connection (plist-get meta :connection))
         (table (opencode-session--prompt-table connection))
         (state (and table (gethash request-id table))))
    (when state
      (setf (opencode-session--prompt-state-status state) 'resolved)
      (opencode-session--remove-prompt state)
      (when (opencode-session--prompt-active-p state)
        (message "OpenCode request answered in another client")
        (abort-recursive-edit)))))

;;; Form handling (v2 replacement for questions)

(defun opencode-session--form-fields (form)
  "Normalize the field list of FORM into a list."
  (let ((fields (alist-get 'fields form)))
    (cond
     ((vectorp fields) (append fields nil))
     ((listp fields) fields)
     (t nil))))

(defun opencode-session--form-option-labels (field)
  "Return an alist of (LABEL . VALUE) for FIELD options."
  (delq nil
        (mapcar (lambda (option)
                  (when (listp option)
                    (cons (or (alist-get 'label option)
                              (alist-get 'value option))
                          (alist-get 'value option))))
                (opencode-session--normalize-items
                 (alist-get 'options field)))))

(defun opencode-session--form-read-field (field)
  "Prompt for FIELD and return (KEY . VALUE), or nil to skip.
Hidden fields are skipped; the server falls back to their default."
  (let ((key (alist-get 'key field)))
    (when (and (stringp key) (not (alist-get 'hidden field)))
      (let* ((title (or (alist-get 'title field) key))
             (description (alist-get 'description field))
             (prompt (if (and (stringp description)
                              (not (string-empty-p description)))
                         (format "OpenCode %s (%s): " title description)
                       (format "OpenCode %s: " title)))
             (type (alist-get 'type field))
             (choices (opencode-session--form-option-labels field))
             (custom (alist-get 'custom field))
             (required (alist-get 'required field))
             (default (alist-get 'default field)))
        (let ((value
               (cond
                ((string= type "boolean")
                 (if (y-or-n-p prompt) t :json-false))
                ((member type '("number" "integer"))
                 (read-number prompt (and (numberp default) default)))
                ((string= type "multiselect")
                 (let* ((labels (mapcar #'car choices))
                        (all (if custom (append labels '("Other")) labels))
                        (selection (completing-read-multiple
                                    prompt all nil (not custom))))
                   (append (delq nil
                                 (mapcar (lambda (label)
                                           (cdr (assoc label choices)))
                                         (remove "Other" selection)))
                           (when (and custom (member "Other" selection))
                             (list (read-string (concat prompt "(Other): ")))))))
                (choices
                 (let* ((labels (mapcar #'car choices))
                        (all (if custom (append labels '("Other")) labels))
                        (selection (completing-read
                                    prompt all nil (not custom) nil nil
                                    (and (stringp default) default))))
                   (if (and custom (string= selection "Other"))
                       (read-string (concat prompt "(Other): "))
                     (cdr (assoc selection choices)))))
                (t
                 (read-string prompt nil nil
                              (and (stringp default) default))))))
          (when (and required
                     (or (null value)
                         (and (stringp value) (string-empty-p value))
                         (and (listp value) (null value))))
            (error "OpenCode: %s is required" key))
          (cons (intern key) value))))))

(defun opencode-session--prompt-form (form connection state)
  "Prompt for FORM fields and send the answer via CONNECTION.
STATE tracks whether another client resolves the request."
  (let* ((form-id (alist-get 'id form))
         (session-id (alist-get 'sessionID form))
         (title (alist-get 'title form))
         (answer
          (let ((opencode-session--active-prompt state))
            (condition-case nil
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local opencode-session--minibuffer-prompt state))
                  (when title
                    (message "OpenCode: %s" title))
                  (delq nil
                        (mapcar #'opencode-session--form-read-field
                                (opencode-session--form-fields form))))
              (quit (unless (eq (opencode-session--prompt-state-status state)
                                'resolved)
                      :cancel))))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless form-id
      (error "OpenCode form request is missing ID"))
    (unless session-id
      (error "OpenCode form request is missing session ID"))
    (unless (eq (opencode-session--prompt-state-status state) 'resolved)
      (setf (opencode-session--prompt-state-status state) 'answered)
      (opencode-session--remove-prompt state)
      (if (eq answer :cancel)
          (opencode-client-form-cancel
           connection
           session-id
           form-id
           :success (lambda (&rest _args)
                      (message "OpenCode form cancelled"))
           :error (lambda (&rest _args)
                    (message "OpenCode: failed to cancel form")))
        (opencode-client-form-reply
         connection
         session-id
         form-id
         answer
         :success (lambda (&rest _args)
                    (message "OpenCode form reply sent"))
         :error (lambda (&rest _args)
                  (message "OpenCode: failed to reply to form")))))))

(defun opencode-session--handle-form-created (_event data meta)
  "Handle a form.created SSE DATA arriving with META.
Routes and defers on a timer so `completing-read' does
not block the process filter."
  (let* ((form (alist-get 'form (alist-get 'properties data)))
         (session-id (alist-get 'sessionID form))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt
                 connection (alist-get 'id form))))
    (when state
      (run-at-time 0 nil
       (lambda ()
         (when (eq (opencode-session--prompt-state-status state) 'pending)
           (opencode-session--run-prompt
            state
            (lambda ()
              (let ((buffer (or (opencode-session--buffer-for-session session-id)
                                (opencode-session--any-live-session-buffer connection))))
                (with-current-buffer (if (buffer-live-p buffer)
                                         buffer
                                       (current-buffer))
                  (opencode-session--prompt-form form connection state)))))))))))

;;; Session event handlers

(defun opencode-session--handle-session-created (_event data)
  "Handle the session.created SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (info (or (alist-get 'info properties) properties))
         (session-id (alist-get 'id info)))
    (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-session-updated (_event data)
  "Handle the session.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'id info)))
    (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--update-session info))))))

(defun opencode-session--handle-model-selected (_event data)
  "Handle the session.model.selected SSE DATA.

The payload carries the session's new `model' reference, so this is
the authoritative refresh point for the model shown in the header."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (model (alist-get 'model properties)))
    (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--set-server-model model)
          (opencode-session--render-header))))))

(defun opencode-session--handle-agent-selected (_event data)
  "Handle the session.agent.selected SSE DATA.

The payload carries the session's new agent name, so this is the
authoritative refresh point for the agent shown in the header."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (agent (alist-get 'agent properties)))
    (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--set-server-agent agent)
          (opencode-session--render-header))))))

(defun opencode-session--status-from-info (status-info)
  "Build an `opencode-status' struct from STATUS-INFO alist.

STATUS-INFO is the `status' field of a session.status SSE payload."
  (let* ((type (or (alist-get 'type status-info) "idle"))
         (attempt (alist-get 'attempt status-info))
         (msg (alist-get 'message status-info))
         (next (alist-get 'next status-info)))
    (opencode-status-create
     :type type
     :attempt (and (numberp attempt) attempt)
     :message (and (stringp msg) (not (string-empty-p msg)) msg)
     :next (and (numberp next) next))))

(defun opencode-session--handle-session-status (_event data)
  "Handle the session.status SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (status-info (alist-get 'status properties))
         (status (opencode-session--status-from-info status-info)))
    (opencode-session--update-status session-id status)))

(defun opencode-session--handle-session-idle (_event data)
  "Handle the session.idle SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (status (opencode-status-create :type "idle")))
    (opencode-session--update-status session-id status)))

(defun opencode-session--session-error-text (error-info)
  "Return user-facing text extracted from session ERROR-INFO."
  (let* ((data (and (listp error-info) (alist-get 'data error-info)))
         (detail (and (listp data) (alist-get 'message data))))
    (cond
     ((and (stringp detail) (not (string-empty-p detail))) detail)
     ((and (listp error-info)
           (stringp (alist-get 'name error-info))
           (not (string-empty-p (alist-get 'name error-info))))
      (alist-get 'name error-info))
     ((stringp error-info) error-info)
     (t "An error occurred"))))

(defun opencode-session--handle-session-error (_event data)
  "Handle the session.error SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (error-info (alist-get 'error properties))
         (error-name (and (listp error-info) (alist-get 'name error-info))))
    (unless (and (stringp error-name)
                 (string= error-name "MessageAbortedError"))
      (message "OpenCode: %s" (opencode-session--session-error-text error-info))
      (when session-id
        (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode-session--render-header))))))))

;;; Message event handlers

(defun opencode-session--handle-message-updated (_event data)
  "Handle the message.updated SSE DATA."
  (let* ((info (alist-get 'info (alist-get 'properties data)))
         (session-id (alist-get 'sessionID info)))
    (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (opencode-session--upsert-message info))))))

(defun opencode-session--handle-message-part-updated (_event data)
  "Handle the message.part.updated SSE DATA.
Routes events to the owning session buffer.  When the session has
no buffer but is a registered subagent, update the subagent tool
tracking and re-render the parent task tool part."
  (let* ((properties (alist-get 'properties data))
         (part (alist-get 'part properties))
         (session-id (alist-get 'sessionID part))
         (delta (alist-get 'delta properties)))
    (if-let* ((buffer (opencode-session--buffer-for-session session-id)))
        ;; Normal case: session has a buffer
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (member (alist-get 'type part) '("text" "tool" "reasoning" "compaction"))
              (opencode-session--update-message-part part delta))))
      ;; Subagent case: track tool calls and re-render parent
      (when (and (string= (alist-get 'type part) "tool")
                 (opencode-session--subagent-parent session-id))
        (opencode-session--track-subagent-tool session-id part)))))

(defun opencode-session--handle-message-part-delta (_event data)
  "Handle the message.part.delta SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'messageID properties))
         (part-id (alist-get 'partID properties))
         (field (alist-get 'field properties))
         (delta (alist-get 'delta properties)))
    (when (and (string= field "text")
               (stringp delta))
      (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((message (opencode-session--find-message message-id)))
              (when-let* ((entry (assoc part-id (opencode-message-parts message)))
                          (part (cdr entry))
                          ((opencode-message-part-p part)))
                (setf (opencode-message-part-text part)
                      (concat (or (opencode-message-part-text part) "") delta))
                (setf (opencode-message-text message)
                      (opencode-session--message-text message))
                (opencode-session--render-message message)))))))))

;;; V2 streaming event handlers.
;; The v2 server replaced `message.*' events with a step/text/tool
;; event family keyed by assistant message ID.  These handlers
;; synthesize server-style part alists and reuse
;; `opencode-session--update-message-part' so rendering is unchanged.

(defun opencode-session--stream-buffer-for (session-id)
  "Return the live session buffer for SESSION-ID, or nil."
  (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
    (when (buffer-live-p buffer)
      buffer)))

(defun opencode-session--handle-step-started (_event data)
  "Ensure the assistant message shell for a step.started event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'assistantMessageID properties)))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (let ((message (opencode-session--ensure-stream-message
                          session-id message-id "assistant"))
                (model (alist-get 'model properties)))
            (setf (opencode-message-agent message)
                  (alist-get 'agent properties))
            (setf (opencode-message-model-id message)
                  (or (alist-get 'modelID model) (alist-get 'id model)))
            (setf (opencode-message-provider-id message)
                  (alist-get 'providerID model))
            (opencode-session--render-header)))))))

(defun opencode-session--handle-step-ended (_event data)
  "Record the finish reason for a step.ended event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'assistantMessageID properties))
         (finish (alist-get 'finish properties)))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (when-let* ((message (opencode-session--find-message message-id)))
            (setf (opencode-message-finish message) finish)
            (opencode-session--render-message message)))))))

(defun opencode-session--stream-text (properties kind)
  "Upsert KIND text for v2 PROPERTIES.
KIND is \"text\" or \"reasoning\".  Delta fragments are appended;
a full `text' value replaces the streamed value."
  (let* ((session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'assistantMessageID properties))
         (ordinal (alist-get 'ordinal properties))
         (delta (alist-get 'delta properties))
         (full (alist-get 'text properties)))
    (when (and session-id message-id (numberp ordinal))
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (let ((message (opencode-session--ensure-stream-message
                          session-id message-id))
                (part-id (opencode-session--stream-text-part-id
                          message-id kind ordinal)))
            (cond
             ;; Seed a new part with the first fragment; later
             ;; fragments append through the update path.
             ((and (stringp delta)
                   (not (assoc part-id (opencode-message-parts message))))
              (opencode-session--upsert-stream-part
               session-id message-id part-id kind `((text . ,delta))))
             ((stringp delta)
              (opencode-session--upsert-stream-part
               session-id message-id part-id kind nil delta))
             ((stringp full)
              (opencode-session--upsert-stream-part
               session-id message-id part-id kind `((text . ,full)))))))))))

(defun opencode-session--handle-text-started (_event data)
  "Handle a session.text.started event."
  (opencode-session--stream-text (alist-get 'properties data) "text"))

(defun opencode-session--handle-text-delta (_event data)
  "Handle a session.text.delta event."
  (opencode-session--stream-text (alist-get 'properties data) "text"))

(defun opencode-session--handle-text-ended (_event data)
  "Handle a session.text.ended event."
  (opencode-session--stream-text (alist-get 'properties data) "text"))

(defun opencode-session--handle-reasoning-started (_event data)
  "Handle a session.reasoning.started event."
  (opencode-session--stream-text (alist-get 'properties data) "reasoning"))

(defun opencode-session--handle-reasoning-delta (_event data)
  "Handle a session.reasoning.delta event."
  (opencode-session--stream-text (alist-get 'properties data) "reasoning"))

(defun opencode-session--handle-reasoning-ended (_event data)
  "Handle a session.reasoning.ended event."
  (opencode-session--stream-text (alist-get 'properties data) "reasoning"))

(defun opencode-session--merge-part-state (old-state extra)
  "Merge EXTRA into OLD-STATE alists, with EXTRA winning."
  (append extra
          (cl-remove-if (lambda (cell)
                          (and (consp cell) (assq (car cell) extra)))
                        (or old-state nil))))

(defun opencode-session--upsert-tool-state (session-id message-id part-id name extra-state)
  "Upsert tool PART-ID for MESSAGE-ID merging EXTRA-STATE.
NAME fills in the tool name when the event omits it.  The caller
must be in the session buffer with the message shell ensured."
  (let* ((message (opencode-session--ensure-stream-message session-id message-id))
         (previous (cdr (assoc part-id (opencode-message-parts message))))
         (old-state (and (opencode-message-part-p previous)
                         (opencode-message-part-state previous)))
         (old-name (and (opencode-message-part-p previous)
                        (opencode-message-part-tool previous))))
    (opencode-session--upsert-stream-part
     session-id message-id part-id "tool"
     `((tool . ,(or name old-name))
       (state . ,(opencode-session--merge-part-state old-state extra-state))))))

(defun opencode-session--with-tool-event (properties fn)
  "Run FN in the session buffer for tool PROPERTIES.
FN is called with SESSION-ID, MESSAGE-ID, and PART-ID."
  (let ((session-id (alist-get 'sessionID properties))
        (message-id (alist-get 'assistantMessageID properties))
        (part-id (alist-get 'id properties)))
    (when (and session-id message-id part-id)
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (funcall fn session-id message-id part-id))))))

(defun opencode-session--handle-tool-input-started (_event data)
  "Handle a session.tool.input.started event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (opencode-session--upsert-tool-state
        session-id message-id part-id
        (alist-get 'name properties)
        '((status . "running")))))))

(defun opencode-session--handle-tool-input-delta (_event data)
  "Handle a session.tool.input.delta event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (let ((delta (alist-get 'delta properties)))
         (when (stringp delta)
           (let* ((message (opencode-session--ensure-stream-message
                            session-id message-id))
                  (previous (cdr (assoc part-id
                                        (opencode-message-parts message))))
                  (old-state (and (opencode-message-part-p previous)
                                   (opencode-message-part-state previous)))
                  (input (alist-get 'input old-state)))
             (opencode-session--upsert-tool-state
              session-id message-id part-id nil
              (list (cons 'status "running")
                    (cons 'input (concat (if (stringp input) input "")
                                         delta)))))))))))

(defun opencode-session--handle-tool-input-ended (_event data)
  "Handle a session.tool.input.ended event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (opencode-session--upsert-tool-state
        session-id message-id part-id nil
        (list (cons 'status "running")
              (cons 'input (alist-get 'text properties))))))))

(defun opencode-session--handle-tool-called (_event data)
  "Handle a session.tool.called event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (opencode-session--upsert-tool-state
        session-id message-id part-id nil
        (list (cons 'status "running")
              (cons 'input (alist-get 'input properties))))
       (opencode-session--register-task-subagent session-id message-id part-id
                                                 (alist-get 'input properties))))))

(defun opencode-session--tool-output-text (content)
  "Return joined text from tool result CONTENT items."
  (string-join (cl-loop for item in (opencode-session--normalize-items content)
                        when (stringp (alist-get 'text item))
                        collect (alist-get 'text item))
               ""))

(defun opencode-session--handle-tool-success (_event data)
  "Handle a session.tool.success event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (let ((output (opencode-session--tool-output-text
                      (alist-get 'content properties))))
         (opencode-session--upsert-tool-state
          session-id message-id part-id nil
          (append '((status . "completed"))
                  (unless (string-empty-p output)
                    (list (cons 'output output))))))))))

(defun opencode-session--handle-tool-failed (_event data)
  "Handle a session.tool.failed event."
  (let ((properties (alist-get 'properties data)))
    (opencode-session--with-tool-event
     properties
     (lambda (session-id message-id part-id)
       (opencode-session--upsert-tool-state
        session-id message-id part-id nil
        (list (cons 'status "error")
              (cons 'error (opencode-session--session-error-text
                            (alist-get 'error properties)))))))))

(defun opencode-session--register-task-subagent (session-id _message-id part-id input)
  "Register a task tool PART-ID as a subagent when INPUT names a session."
  (let ((subagent-id (and (listp input)
                          (or (alist-get 'sessionId input)
                              (alist-get 'sessionID input)))))
    (when (and (stringp subagent-id) (not (string-empty-p subagent-id)))
      (opencode-session--register-subagent subagent-id session-id part-id))))

(defun opencode-session--handle-content-updated (_event data)
  "Reconcile message text from a session.message.content.updated event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (alist-get 'messageID properties))
         (content (alist-get 'content properties)))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (opencode-session--ensure-stream-message session-id message-id)
          (let ((counters (make-hash-table :test #'equal)))
            (dolist (item (opencode-session--normalize-items content))
              (let ((type (alist-get 'type item)))
                (cond
                 ((member type '("text" "reasoning"))
                  (let* ((count (1+ (gethash type counters 0)))
                         (_ (puthash type count counters))
                         (part-id (format "%s:%s:%d" message-id type (1- count))))
                    (opencode-session--upsert-stream-part
                     session-id message-id part-id type
                     (list (cons 'text (alist-get 'text item))))))
                 ((string= type "tool")
                  (opencode-session--upsert-tool-state
                   session-id message-id (alist-get 'id item)
                   (alist-get 'name item)
                   (list (cons 'status "completed")
                         (cons 'input (alist-get 'input item))))))))))))))

(defun opencode-session--handle-execution-started (_event data)
  "Mark the session busy for a session.execution.started event."
  (let ((session-id (alist-get 'sessionID (alist-get 'properties data))))
    (when session-id
      (opencode-session--update-status
       session-id (opencode-status-create :type "busy")))))

(defun opencode-session--handle-execution-ended (_event data status-type)
  "Mark the session STATUS-TYPE for a terminal execution event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties)))
    (when session-id
      (opencode-session--update-status
       session-id (opencode-status-create :type status-type))
      (when-let* ((error-info (alist-get 'error properties)))
        (message "OpenCode: %s"
                 (opencode-session--session-error-text error-info))))))

(defun opencode-session--handle-execution-succeeded (_event data)
  "Handle a session.execution.succeeded event."
  (opencode-session--handle-execution-ended _event data "idle"))

(defun opencode-session--handle-execution-failed (_event data)
  "Handle a session.execution.failed event."
  (opencode-session--handle-execution-ended _event data "idle"))

(defun opencode-session--handle-execution-interrupted (_event data)
  "Handle a session.execution.interrupted event."
  (opencode-session--handle-execution-ended _event data "idle"))

(defun opencode-session--handle-session-renamed (_event data)
  "Handle a session.renamed event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (title (alist-get 'title properties)))
    (when session-id
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (let ((previous-name (and opencode-session--session
                                    (opencode-session--buffer-name
                                     opencode-session--session))))
            (unless opencode-session--session
              (setq opencode-session--session
                    (opencode-session-create :id session-id)))
            (setf (opencode-session-title opencode-session--session) title)
            (opencode-session--rename-buffer previous-name)
            (opencode-session--render-header)))))))

(defun opencode-session--handle-session-moved (_event data)
  "Handle a session.moved event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (directory (or (alist-get 'directory properties)
                        (alist-get 'directory
                                   (alist-get 'location properties)))))
    (when session-id
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (with-current-buffer buffer
          (when opencode-session--session
            (setf (opencode-session-directory opencode-session--session)
                  directory)
            (opencode-session--render-header)))))))

(defun opencode-session--handle-compaction-failed (_event data)
  "Handle a session.compaction.failed event."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (or (alist-get 'messageID properties)
                         (alist-get 'inputID properties)
                         session-id))
         (reason (or (alist-get 'reason properties) "manual")))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--stream-buffer-for session-id)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (opencode-session--upsert-compaction session-id message-id reason t)
            (when-let* ((error-info (alist-get 'error properties)))
              (message "OpenCode: %s"
                       (opencode-session--session-error-text error-info)))))))))

(defun opencode-session--handle-compaction-started (_event data)
  "Handle session compaction started SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (or (alist-get 'messageID properties)
                         (alist-get 'inputID properties)
                         session-id))
         (reason (or (alist-get 'reason properties) "manual")))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (opencode-session--upsert-compaction session-id message-id reason)))))))

(defun opencode-session--handle-compaction-ended (_event data)
  "Handle session compaction ended SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (session-id (alist-get 'sessionID properties))
         (message-id (or (alist-get 'messageID properties)
                         (alist-get 'inputID properties)
                         session-id))
         (reason (or (alist-get 'reason properties) "manual"))
         (timestamp (or (alist-get 'timestamp properties) t)))
    (when (and session-id message-id)
      (when-let* ((buffer (opencode-session--buffer-for-session session-id)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (opencode-session--upsert-compaction session-id message-id reason timestamp)))))))

;;; Subagent tracking

(defun opencode-session--track-subagent-tool (subagent-session-id part)
  "Track a tool PART from SUBAGENT-SESSION-ID and re-render the parent."
  (let* ((part-id (alist-get 'id part))
         (tool (alist-get 'tool part))
         (state (alist-get 'state part))
         (parent-info (opencode-session--subagent-parent subagent-session-id)))
    (when (and parent-info tool)
      (opencode-session--update-subagent-tool
       subagent-session-id part-id tool state)
      ;; Re-render the task tool part in the parent session buffer
      (let* ((parent-session-id (car parent-info))
             (parent-buffer (opencode-session--buffer-for-session parent-session-id)))
        (when (and parent-buffer (buffer-live-p parent-buffer))
          (with-current-buffer parent-buffer
            (opencode-session--rerender-task-part parent-info)))))))

(defun opencode-session--rerender-task-part (parent-info)
  "Re-render the task tool part identified by PARENT-INFO.
PARENT-INFO is a cons (PARENT-SESSION-ID . TASK-PART-ID)."
  (let ((task-part-id (cdr parent-info)))
    (when-let* ((message (opencode-session--find-message-by-part task-part-id)))
      (opencode-session--render-message message))))

(defun opencode-session--find-message-by-part (part-id)
  "Find the message containing PART-ID in the current buffer."
  (cl-find-if (lambda (message)
                (assoc part-id (opencode-message-parts message)))
              opencode-session--messages))

(defun opencode-session--maybe-register-subagent (part parent-session-id)
  "Register subagent mapping if PART is a task tool with a session ID.
PARENT-SESSION-ID is the session that owns the task tool part."
  (when (and (string= (alist-get 'type part) "tool")
             (string= (alist-get 'tool part) "task"))
    (let* ((state (alist-get 'state part))
           (metadata (alist-get 'metadata state))
           (subagent-session-id (alist-get 'sessionId metadata))
           (part-id (alist-get 'id part)))
      (when subagent-session-id
        (opencode-session--register-subagent
         subagent-session-id parent-session-id part-id)))))

;;; Permission handling

(defun opencode-session--permission-patterns (permission)
  "Return a list of pattern strings from PERMISSION."
  (let ((patterns (alist-get 'patterns permission)))
    (cond
     ((vectorp patterns) (append patterns nil))
     ((listp patterns) patterns)
     (t nil))))

(defun opencode-session--permission-detail (permission)
  "Return a detail string for PERMISSION when available."
  (let* ((kind (alist-get 'permission permission))
         (metadata (alist-get 'metadata permission))
         (patterns (opencode-session--permission-patterns permission))
         (pattern (car patterns)))
    (cond
     ((and (string= kind "read") (alist-get 'filePath metadata))
      (format "read %s" (alist-get 'filePath metadata)))
     ((and (string= kind "edit") (alist-get 'filepath metadata))
      (format "edit %s" (alist-get 'filepath metadata)))
     ((and (string= kind "glob") (alist-get 'pattern metadata))
      (format "glob %s" (alist-get 'pattern metadata)))
     ((and (string= kind "grep") (alist-get 'pattern metadata))
      (format "grep %s" (alist-get 'pattern metadata)))
     ((and (string= kind "list") (alist-get 'path metadata))
      (format "list %s" (alist-get 'path metadata)))
     ((and (string= kind "bash") (alist-get 'command metadata))
      (if-let* ((description (alist-get 'description metadata)))
          (format "%s (%s)" description (alist-get 'command metadata))
        (format "%s" (alist-get 'command metadata))))
     ((and (string= kind "task") (alist-get 'subagent_type metadata))
      (format "task %s" (alist-get 'subagent_type metadata)))
     ((and (string= kind "webfetch") (alist-get 'url metadata))
      (format "web search %s" (alist-get 'url metadata)))
     ((and (member kind '("websearch" "codesearch")) (alist-get 'query metadata))
      (format "%s %s" (capitalize kind) (alist-get 'query metadata)))
     ((and (string= kind "external_directory") pattern)
      (format "access external directory %s" pattern))
     (pattern
      (format "%s" pattern))
     (t nil))))

(defun opencode-session--permission-prompt-label (permission)
  "Return the minibuffer prompt label for PERMISSION."
  (let* ((kind (alist-get 'permission permission))
         (detail (opencode-session--permission-detail permission))
         (fallback (if kind (format "use %s" kind) "proceed")))
    (format "OpenCode wants to %s: " (or detail fallback))))

(defun opencode-session--prompt-permission (permission connection state)
  "Prompt for PERMISSION and send a response via CONNECTION.
STATE tracks whether another client resolves the request."
  (let* ((request-id (alist-get 'id permission))
         (session-id (alist-get 'sessionID permission))
         (choices '("Allow once" "Allow always" "Deny"))
         (prompt (opencode-session--permission-prompt-label permission))
         (selection
          (let ((opencode-session--active-prompt state))
            (condition-case nil
                (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local opencode-session--minibuffer-prompt state))
                  (completing-read prompt choices nil t))
              (quit (unless (eq (opencode-session--prompt-state-status state)
                                'resolved)
                      "Deny")))))
         (reply (cond
                  ((equal selection "Allow always") "always")
                  ((equal selection "Allow once") "once")
                  (t "reject"))))
    (unless connection
      (error "OpenCode session is not connected"))
    (unless request-id
      (error "OpenCode permission request is missing ID"))
    (unless session-id
      (error "OpenCode permission request is missing session ID"))
    (unless (eq (opencode-session--prompt-state-status state) 'resolved)
      (setf (opencode-session--prompt-state-status state) 'answered)
      (opencode-session--remove-prompt state)
      (opencode-client-permission-reply
       connection
       request-id
       reply
       session-id
       :success (lambda (&rest _args)
                  (message "OpenCode permission reply sent"))
       :error (lambda (&rest _args)
                (message "OpenCode: failed to reply to permission request"))))))

(defun opencode-session--handle-permission-asked (_event data meta)
  "Handle the permission.asked SSE DATA arriving with META.
META is a plist carrying `:connection', the connection on which
the event arrived; the reply is always routed back to that
connection so multi-server setups respond to the correct server.

Falls back to any live session buffer on the same connection when
SESSION-ID is unknown, e.g. for permission requests originating
from subagent sessions.  Defers to a timer so `completing-read'
does not block the process filter."
   (let* ((properties (alist-get 'properties data))
         ;; V2 asks carry {action, resources}; present them with the
         ;; v1 vocabulary the prompt UI reads ({permission, patterns}).
         (permission (if (assq 'permission properties)
                         properties
                       (append (list (cons 'permission
                                           (alist-get 'action properties))
                                     (cons 'patterns
                                           (alist-get 'resources properties)))
                               properties)))
         (session-id (alist-get 'sessionID permission))
         (connection (plist-get meta :connection))
         (state (opencode-session--register-prompt
                 connection (alist-get 'id permission))))
    (when state
      (run-at-time 0 nil
       (lambda ()
         (when (eq (opencode-session--prompt-state-status state) 'pending)
           (opencode-session--run-prompt
            state
            (lambda ()
              (let ((buffer (or (opencode-session--buffer-for-session session-id)
                                (opencode-session--any-live-session-buffer connection))))
                (if (and buffer (buffer-live-p buffer))
                    (with-current-buffer buffer
                      (opencode-session--prompt-permission permission connection state))
                  ;; No buffer to host the prompt, but we can still reply via
                  ;; the originating connection.
                  (opencode-session--prompt-permission permission connection state)))))))))))

;;; File revert handling

(defun opencode-session--connection-directories ()
  "Return directories for active OpenCode connections."
  (let (directories)
    (maphash (lambda (_session-id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when-let* ((connection opencode-session--connection)
                              (directory (opencode-connection-directory connection)))
                     (push directory directories)))))
             opencode-session--buffers)
    (delete-dups (delq nil directories))))

(defun opencode-session--normalize-file-path (path)
  "Normalize PATH for buffer lookup.

Returns nil when PATH is not a string."
  (when (stringp path)
    (let* ((expanded (expand-file-name path))
           (directories (opencode-session--connection-directories)))
      (cond
       ((file-name-absolute-p expanded)
        (if (file-exists-p expanded)
            (file-truename expanded)
          expanded))
       (directories
        (let ((candidate (cl-find-if
                          #'file-exists-p
                          (mapcar (lambda (directory)
                                    (expand-file-name path directory))
                                  directories))))
          (if candidate
              (file-truename candidate)
            (expand-file-name path (car directories)))))
       (t expanded)))))

(defun opencode-session--event-file-paths (data)
  "Return a list of file paths from SSE DATA."
  (let* ((properties (alist-get 'properties data))
         (file (alist-get 'file properties))
         (path (alist-get 'path properties))
         (file-path (cond
                     ((stringp file) file)
                     ((listp file) (or (alist-get 'path file)
                                       (alist-get 'name file)))))
         (paths (or (alist-get 'paths properties)
                    (alist-get 'files properties))))
    (cond
     ((and paths (vectorp paths)) (append paths nil))
     ((listp paths) paths)
     ((stringp path) (list path))
     ((stringp file-path) (list file-path))
     (t nil))))

(defun opencode-session--maybe-revert-buffer (path)
  "Revert buffers visiting PATH when safe."
  (let ((normalized (opencode-session--normalize-file-path path)))
    (when normalized
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when-let* ((buffer-path (buffer-file-name buffer)))
              (let ((normalized-buffer (opencode-session--normalize-file-path buffer-path)))
                (when (and normalized-buffer
                           (string= normalized normalized-buffer))
                  (if (buffer-modified-p)
                      (message "OpenCode: buffer has unsaved changes (%s)" (buffer-name buffer))
                    (revert-buffer :ignore-auto :noconfirm)
                    (message "OpenCode: reloaded %s" (buffer-name buffer))))))))))))

(defun opencode-session--handle-file-updated (_event data)
  "Handle SSE file update DATA by reverting buffers."
  (dolist (path (opencode-session--event-file-paths data))
    (opencode-session--maybe-revert-buffer path)))

;;; Handler registrations

(opencode-sse-define-handler session-created "session.created" (_event data _meta)
  (opencode-session--handle-session-created _event data))

(opencode-sse-define-handler session-updated "session.updated" (_event data _meta)
  (opencode-session--handle-session-updated _event data))

(opencode-sse-define-handler model-selected "session.model.selected" (_event data _meta)
  (opencode-session--handle-model-selected _event data))

(opencode-sse-define-handler agent-selected "session.agent.selected" (_event data _meta)
  (opencode-session--handle-agent-selected _event data))

(opencode-sse-define-handler session-renamed "session.renamed" (_event data _meta)
  (opencode-session--handle-session-renamed _event data))

(opencode-sse-define-handler session-moved "session.moved" (_event data _meta)
  (opencode-session--handle-session-moved _event data))

(opencode-sse-define-handler session-status "session.status" (_event data _meta)
  (opencode-session--handle-session-status _event data))

(opencode-sse-define-handler session-idle "session.idle" (_event data _meta)
  (opencode-session--handle-session-idle _event data))

(opencode-sse-define-handler session-error "session.error" (_event data _meta)
  (opencode-session--handle-session-error _event data))

(opencode-sse-define-handler permission-asked "permission.asked" (_event data meta)
  (opencode-session--handle-permission-asked _event data meta))

(opencode-sse-define-handler permission-replied "permission.replied" (_event data meta)
  (opencode-session--handle-prompt-resolved _event data meta))

(opencode-sse-define-handler form-created "form.created" (_event data meta)
  (opencode-session--handle-form-created _event data meta))

(opencode-sse-define-handler form-replied "form.replied" (_event data meta)
  (opencode-session--handle-prompt-resolved _event data meta))

(opencode-sse-define-handler form-cancelled "form.cancelled" (_event data meta)
  (opencode-session--handle-prompt-resolved _event data meta))

(opencode-sse-define-handler message-updated "message.updated" (_event data _meta)
  (opencode-session--handle-message-updated _event data))

(opencode-sse-define-handler message-part-updated "message.part.updated" (_event data _meta)
  (opencode-session--handle-message-part-updated _event data))

(opencode-sse-define-handler message-part-delta "message.part.delta" (_event data _meta)
  (opencode-session--handle-message-part-delta _event data))

(opencode-sse-define-handler compaction-started "session.next.compaction.started" (_event data _meta)
  (opencode-session--handle-compaction-started _event data))

(opencode-sse-define-handler compaction-ended "session.next.compaction.ended" (_event data _meta)
  (opencode-session--handle-compaction-ended _event data))

(opencode-sse-define-handler step-started "session.step.started" (_event data _meta)
  (opencode-session--handle-step-started _event data))

(opencode-sse-define-handler step-ended "session.step.ended" (_event data _meta)
  (opencode-session--handle-step-ended _event data))

(opencode-sse-define-handler text-started "session.text.started" (_event data _meta)
  (opencode-session--handle-text-started _event data))

(opencode-sse-define-handler text-delta "session.text.delta" (_event data _meta)
  (opencode-session--handle-text-delta _event data))

(opencode-sse-define-handler text-ended "session.text.ended" (_event data _meta)
  (opencode-session--handle-text-ended _event data))

(opencode-sse-define-handler reasoning-started "session.reasoning.started" (_event data _meta)
  (opencode-session--handle-reasoning-started _event data))

(opencode-sse-define-handler reasoning-delta "session.reasoning.delta" (_event data _meta)
  (opencode-session--handle-reasoning-delta _event data))

(opencode-sse-define-handler reasoning-ended "session.reasoning.ended" (_event data _meta)
  (opencode-session--handle-reasoning-ended _event data))

(opencode-sse-define-handler tool-input-started "session.tool.input.started" (_event data _meta)
  (opencode-session--handle-tool-input-started _event data))

(opencode-sse-define-handler tool-input-delta "session.tool.input.delta" (_event data _meta)
  (opencode-session--handle-tool-input-delta _event data))

(opencode-sse-define-handler tool-input-ended "session.tool.input.ended" (_event data _meta)
  (opencode-session--handle-tool-input-ended _event data))

(opencode-sse-define-handler tool-called "session.tool.called" (_event data _meta)
  (opencode-session--handle-tool-called _event data))

(opencode-sse-define-handler tool-success "session.tool.success" (_event data _meta)
  (opencode-session--handle-tool-success _event data))

(opencode-sse-define-handler tool-failed "session.tool.failed" (_event data _meta)
  (opencode-session--handle-tool-failed _event data))

(opencode-sse-define-handler content-updated "session.message.content.updated" (_event data _meta)
  (opencode-session--handle-content-updated _event data))

(opencode-sse-define-handler execution-started "session.execution.started" (_event data _meta)
  (opencode-session--handle-execution-started _event data))

(opencode-sse-define-handler execution-succeeded "session.execution.succeeded" (_event data _meta)
  (opencode-session--handle-execution-succeeded _event data))

(opencode-sse-define-handler execution-failed "session.execution.failed" (_event data _meta)
  (opencode-session--handle-execution-failed _event data))

(opencode-sse-define-handler execution-interrupted "session.execution.interrupted" (_event data _meta)
  (opencode-session--handle-execution-interrupted _event data))

(opencode-sse-define-handler compaction-v2-started "session.compaction.started" (_event data _meta)
  (opencode-session--handle-compaction-started _event data))

(opencode-sse-define-handler compaction-v2-ended "session.compaction.ended" (_event data _meta)
  (opencode-session--handle-compaction-ended _event data))

(opencode-sse-define-handler compaction-v2-failed "session.compaction.failed" (_event data _meta)
  (opencode-session--handle-compaction-failed _event data))

(opencode-sse-define-handler provider-updated "provider.updated" (_event _data meta)
  (when-let* ((connection (plist-get meta :connection)))
    (opencode-connection-providers-changed connection)))

(opencode-sse-define-handler model-updated "model.updated" (_event _data meta)
  (when-let* ((connection (plist-get meta :connection)))
    (opencode-connection-providers-changed connection)))

(opencode-sse-define-handler agent-updated "agent.updated" (_event _data meta)
  (when-let* ((connection (plist-get meta :connection)))
    (opencode-session--agents-changed connection)))

(opencode-sse-define-handler integration-updated "integration.updated" (_event _data meta)
  (when-let* ((connection (plist-get meta :connection)))
    (opencode-connection-providers-changed connection)))

(opencode-sse-define-handler filesystem-changed "filesystem.changed" (_event data _meta)
  (opencode-session--handle-file-updated _event data))

(opencode-sse-define-handler file-edited "file.edited" (_event data _meta)
  (opencode-session--handle-file-updated _event data))

(opencode-sse-define-handler file-watcher-updated "file.watcher.updated" (_event data _meta)
  (opencode-session--handle-file-updated _event data))

;; TODO: handle additional bus events that the server publishes via
;; `Bus.subscribeAll' on the /event SSE stream:
;;   - `tui.toast.show'  — used by MCP auth flow and any external POST
;;     /tui/toast caller; would be nice to surface in the echo area
;;     with a face based on the variant (info/success/warning/error).
;;   - `installation.update-available' — server announcing a new
;;     opencode release; currently the TUI shows a confirm dialog and
;;     runs the upgrade.

(provide 'emacs-opencode-session-handlers)

;;; emacs-opencode-session-handlers.el ends here
