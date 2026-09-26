;;; emacs-opencode-session-model.el --- Agent, model, and variant selection  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'emacs-opencode-session-vars)
(require 'emacs-opencode-connection)
(require 'emacs-opencode-message)
(require 'emacs-opencode-client)
(require 'emacs-opencode-sse)

(declare-function opencode-session--render-header "emacs-opencode-session-header")
(declare-function opencode-session--session-used-models "emacs-opencode-session-header")
(declare-function opencode-session--maybe-start-spinner "emacs-opencode-session-header")
(declare-function opencode-session--maybe-stop-spinner "emacs-opencode-session-header")
(declare-function opencode-session--ensure-connection "emacs-opencode-session-mode" (callback))
(declare-function opencode-session--form-read-field "emacs-opencode-session-handlers" (field))

(defcustom opencode-session-default-agent "plan"
  "Agent name shown until the server reports an agent for the session.

This is a display fallback only.  The server owns the session agent, so
the value is never sent: pick an agent with `opencode-session-select-agent'
to switch the session for real."
  :type 'string
  :group 'emacs-opencode)

(defcustom opencode-session-default-variant nil
  "Default model variant name for new OpenCode sessions.

When nil, do not auto-select a model variant."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Variant name"))
  :group 'emacs-opencode)

(defvar opencode-session--recent-models nil
  "Global list of recently selected (PROVIDER-ID . MODEL-ID) pairs.
Most recently selected first.  This is an in-memory mirror of the
`recent' list in the TUI's `model.json' preference file; the file is
the source of truth and this cache is seeded from it.")

(defvar opencode-session--favorite-models nil
  "Global list of favorite (PROVIDER-ID . MODEL-ID) pairs.
Mirrors the `favorite' list in the TUI's `model.json' preference file.")

(defvar opencode-session--model-variant-map nil
  "Alist mapping \"PROVIDER-ID/MODEL-ID\" strings to variant names.
Mirrors the `variant' object in the TUI's `model.json' preference file.")

(defvar opencode-session--preferences-loaded nil
  "Non-nil once the model preference file has been read this session.")

(defun opencode-session--preference-file ()
  "Return the path of the TUI's `model.json' preference file.

This lives under the XDG state directory, e.g.
`~/.local/state/opencode/model.json', shared with the TUI and CLI so
model recents, favorites, and per-model variants persist across all
OpenCode clients."
  (let ((state-home (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state"))))
    (expand-file-name "opencode/model.json" state-home)))

(defun opencode-session--preference-key (provider-id model-id)
  "Return the preference-map key for PROVIDER-ID and MODEL-ID."
  (format "%s/%s" provider-id model-id))

(defun opencode-session--ensure-preferences ()
  "Load model preferences from disk once, then return non-nil."
  (unless opencode-session--preferences-loaded
    (setq opencode-session--preferences-loaded t)
    (opencode-session--load-preferences))
  t)

(defun opencode-session--load-preferences ()
  "Seed recents, favorites, and variants from the TUI preference file."
  (let ((file (opencode-session--preference-file)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let* ((data (json-parse-string
                          (buffer-string)
                          :object-type 'alist :array-type 'list
                          :null-object nil :false-object nil))
                   (pair (lambda (item)
                           (let ((provider (alist-get 'providerID item))
                                 (model (alist-get 'modelID item)))
                             (and (stringp provider) (stringp model)
                                  (cons provider model))))))
              (setq opencode-session--recent-models
                    (delq nil (mapcar pair (alist-get 'recent data))))
              (setq opencode-session--favorite-models
                    (delq nil (mapcar pair (alist-get 'favorite data))))
              (setq opencode-session--model-variant-map nil)
              (dolist (entry (alist-get 'variant data))
                (when (and (consp entry) (stringp (cdr entry)))
                  (let ((key (if (stringp (car entry))
                                 (car entry)
                               (symbol-name (car entry)))))
                    (push (cons key (cdr entry))
                          opencode-session--model-variant-map))))))
        (error nil)))))

(defun opencode-session--save-preferences ()
  "Write recents, favorites, and variants back to the preference file.

Uses an atomic rename and retries once when the file changed under us,
so a concurrent TUI write is merged rather than clobbered."
  (opencode-session--ensure-preferences)
  (let ((file (opencode-session--preference-file))
        (done nil))
    (dotimes (_ 2 done)
      (let ((mtime (and (file-exists-p file)
                        (file-attribute-modification-time
                         (file-attributes file)))))
        (condition-case nil
            (progn
              (make-directory (file-name-directory file) t)
              (let ((tmp (make-temp-file "opencode-model" nil ".json")))
                (with-temp-file tmp
                  (insert (json-encode
                           `((recent . ,(mapcar
                                         (lambda (key)
                                           `((providerID . ,(car key))
                                             (modelID . ,(cdr key))))
                                         opencode-session--recent-models))
                             (favorite . ,(mapcar
                                           (lambda (key)
                                             `((providerID . ,(car key))
                                               (modelID . ,(cdr key))))
                                           opencode-session--favorite-models))
                             (variant . ,opencode-session--model-variant-map)))))
                (if (or (null mtime)
                        (not (file-exists-p file))
                        (equal mtime (file-attribute-modification-time
                                      (file-attributes file))))
                    (progn (rename-file tmp file t) (setq done t))
                  (delete-file tmp)
                  (opencode-session--load-preferences))))
          (error (setq done t)))))))

(defun opencode-session--record-recent (provider-id model-id)
  "Record PROVIDER-ID/MODEL-ID as most recently used and persist it."
  (opencode-session--ensure-preferences)
  (let ((key (cons provider-id model-id)))
    (setq opencode-session--recent-models
          (seq-take (cons key (cl-remove key opencode-session--recent-models
                                         :test #'equal))
                    10))
    (opencode-session--save-preferences)))

(defun opencode-session--record-variant (provider-id model-id variant)
  "Remember VARIANT for PROVIDER-ID/MODEL-ID and persist it.
A nil VARIANT clears the remembered mapping, matching the TUI's
explicit \"default\" override."
  (opencode-session--ensure-preferences)
  (let ((key (opencode-session--preference-key provider-id model-id)))
    (setq opencode-session--model-variant-map
          (assoc-delete-all key opencode-session--model-variant-map))
    (when (and (stringp variant) (not (string-empty-p variant)))
      (push (cons key variant) opencode-session--model-variant-map))
    (opencode-session--save-preferences)))

(defun opencode-session--remembered-variant (provider-id model-id)
  "Return the persisted variant for PROVIDER-ID/MODEL-ID, or nil."
  (opencode-session--ensure-preferences)
  (cdr (assoc (opencode-session--preference-key provider-id model-id)
              opencode-session--model-variant-map)))

(defun opencode-session--remembered-model ()
  "Return the model to offer new sessions as (PROVIDER-ID . MODEL-ID).

This is the most recent pick, so a fresh session starts where the
user left off across all OpenCode clients.  Favorites only affect
selector sorting; they are never auto-selected."
  (opencode-session--ensure-preferences)
  (car opencode-session--recent-models))

(defun opencode-session--remembered-model-ref ()
  "Return a `Model.Ref' alist for the remembered model, or nil.

Includes the persisted variant when the remembered model has one."
  (when-let* ((key (opencode-session--remembered-model)))
    (opencode-client--model-ref
     (cdr key) (car key)
     (opencode-session--remembered-variant (car key) (cdr key)))))

(defvar-local opencode-session--agent-index nil
  "Index of the server-selected agent in the available agents list.")

(defvar-local opencode-session--variant-index nil
  "Index of the server-selected variant in the available variants list.")

;;; Server-authoritative selection
;;
;; The OpenCode server owns the session agent and model.  It applies a
;; selection with `POST /api/session/:id/agent' and
;; `POST /api/session/:id/model', records it in the message history as an
;; `agent-switched' or `model-switched' message, and broadcasts a
;; `session.agent.selected' or `session.model.selected' event.  The client
;; keeps no authoritative copy of its own: it seeds the selection from
;; `Session.Info' and the message history when a session loads, and
;; refreshes it from those events.  (`Session.Info' is null for sessions
;; that were never explicitly selected, even after turns have run -- the
;; runner resolves the default lazily without writing it back.)

(defun opencode-session--model-ref-p (ref)
  "Return non-nil when REF looks like a server `Model.Ref' alist."
  (and (consp ref)
       (stringp (alist-get 'id ref))
       (stringp (alist-get 'providerID ref))))

(defun opencode-session--newest-message-info (type)
  "Return the info of the most recent message whose `type' is TYPE.

Compares `time.created' rather than list position: `opencode-session--messages'
mixes newest-first API history with messages appended while streaming."
  (let (best best-time found)
    (dolist (message opencode-session--messages)
      (let ((info (opencode-message-info message)))
        (when (and (consp info) (equal (alist-get 'type info) type))
          (let* ((created (alist-get 'created (alist-get 'time info)))
                 (newer (or (not found)
                            (and (numberp created) (null best-time))
                            (and (numberp created)
                                 (numberp best-time)
                                 (> created best-time)))))
            (when newer
              (setq best info
                    best-time (and (numberp created) created)
                    found t))))))
    best))

(defun opencode-session--message-model-ref (type)
  "Return the model reference carried by the newest message of TYPE."
  (let ((model (alist-get 'model (opencode-session--newest-message-info type))))
    (and (opencode-session--model-ref-p model) model)))

(defun opencode-session--message-agent (type)
  "Return the agent carried by the newest message of TYPE."
  (let ((agent (alist-get 'agent (opencode-session--newest-message-info type))))
    (and (stringp agent) agent)))

(defun opencode-session--set-server-model (ref)
  "Record REF as the server-reported model selection for this buffer."
  (when (opencode-session--model-ref-p ref)
    (setq-local opencode-session--server-model ref)
    (opencode-session--sync-variant-selection)))

(defun opencode-session--set-server-agent (agent)
  "Record AGENT as the server-reported agent for this buffer."
  (when (stringp agent)
    (setq-local opencode-session--server-agent agent)
    (opencode-session--sync-agent-index)))

(defun opencode-session--adopt-selection-from-messages ()
  "Seed the server selection state from the loaded message history."
  (let ((model (opencode-session--message-model-ref "model-switched"))
        (agent (opencode-session--message-agent "agent-switched")))
    (when (opencode-session--model-ref-p model)
      (setq-local opencode-session--server-model model))
    (when (stringp agent)
      (setq-local opencode-session--server-agent agent))
    (opencode-session--sync-agent-index)
    (opencode-session--sync-variant-selection)))

(defun opencode-session--current-model-ref ()
  "Return the server's model reference for this buffer.

Falls back to the model of the newest assistant message when the server
has recorded no explicit selection."
  (or opencode-session--server-model
      (opencode-session--message-model-ref "assistant")))

(defun opencode-session--current-model ()
  "Return the active model as a cons (PROVIDER-ID . MODEL-ID)."
  (let ((ref (opencode-session--current-model-ref)))
    (when (opencode-session--model-ref-p ref)
      (cons (alist-get 'providerID ref) (alist-get 'id ref)))))

(defun opencode-session--variant-of (ref)
  "Return the non-empty variant of the model reference REF, or nil."
  (let ((variant (and (consp ref) (alist-get 'variant ref))))
    (and (stringp variant) (not (string-empty-p variant)) variant)))

(defun opencode-session--current-variant ()
  "Return the variant of the active model, or nil when it has none.

A selection made without an explicit variant still resolves to one, so
fall back to the variant the server recorded on the newest assistant
message for the same model."
  (let* ((ref (opencode-session--current-model-ref))
         (used (opencode-session--message-model-ref "assistant")))
    (or (opencode-session--variant-of ref)
        (and (equal (alist-get 'id used) (alist-get 'id ref))
             (equal (alist-get 'providerID used) (alist-get 'providerID ref))
             (opencode-session--variant-of used)))))

(defun opencode-session--current-agent ()
  "Return the server-selected agent name for this buffer.

Falls back to the agent of the newest assistant message and then to
`opencode-session-default-agent', so the header always names an agent."
  (or opencode-session--server-agent
      (opencode-session--message-agent "assistant")
      opencode-session-default-agent))

(defun opencode-session--sync-agent-index ()
  "Align the agent menu cursor with the server-selected agent."
  (let* ((agents (opencode-session--available-agents))
         (agent opencode-session--server-agent)
         (index (and agent agents
                     (cl-position agent agents :test #'string=))))
    (setq-local opencode-session--agent-index index)))

(defun opencode-session--default-ref-from-info (info)
  "Return a `Model.Ref' alist extracted from default-model INFO.

INFO is a `Model.Info' object (or its `{data: ...}' envelope) as
returned by `GET /api/model/default'.  Returns nil when INFO names no
usable model."
  (let ((model (or (alist-get 'data info) info)))
    (when (and (consp model)
               (stringp (alist-get 'id model))
               (stringp (alist-get 'providerID model)))
      (let ((ref (list (cons 'id (alist-get 'id model))
                       (cons 'providerID (alist-get 'providerID model)))))
        (when-let* ((variant (alist-get 'variant model))
                    ((stringp variant))
                    ((not (string-empty-p variant))))
          (setq ref (append ref (list (cons 'variant variant)))))
        ref))))

(defun opencode-session--resolve-initial-model ()
  "Resolve the model to display for the current session buffer.

Priority order: an already recorded selection wins; then the session's
own info (a session born selected carries it); then the most recent
model (which is also what new sessions are created with); the server
default is the last resort and is only fetched when nothing else named
a model.  Does nothing without a connection."
  (when opencode-session--connection
    (let ((switch (opencode-session--message-model-ref "model-switched"))
          (info (and opencode-session--session
                     (opencode-session-info opencode-session--session)))
          (remembered (opencode-session--remembered-model-ref)))
      (when-let* ((agent (alist-get 'agent info))
                  ((stringp agent))
                  ((null opencode-session--server-agent))
                  ((null (opencode-session--message-agent "agent-switched"))))
        (opencode-session--set-server-agent agent))
      (let ((info-model (alist-get 'model info)))
        (cond
         ((or opencode-session--server-model switch)
          nil)
         ((opencode-session--model-ref-p info-model)
          (opencode-session--set-server-model info-model)
          (opencode-session--render-header))
         ((opencode-session--model-ref-p remembered)
          (opencode-session--set-server-model remembered)
          (opencode-session--render-header))
         (t
          (let ((buffer (current-buffer))
                (connection opencode-session--connection))
            (opencode-client-model-default
             connection
             :success (lambda (&rest args)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (when (and (eq opencode-session--connection connection)
                                       (null opencode-session--server-model)
                                       (null (opencode-session--message-model-ref
                                              "model-switched")))
                              (when-let* ((ref (opencode-session--default-ref-from-info
                                                (plist-get args :data))))
                                (opencode-session--set-server-model ref)
                                (opencode-session--render-header))))))
             :error (lambda (&rest _args) nil)))))))))

(defalias 'opencode-session--fetch-default-model
  #'opencode-session--resolve-initial-model
  "Fetch the server's default model for the current session buffer.
Obsolete alias of `opencode-session--resolve-initial-model', which now
prefers explicit, info, and remembered models first.")

(defun opencode-session--failure-detail (args)
  "Return a parenthesized detail suffix for the failed request ARGS."
  (let ((detail (opencode-client-format-error args)))
    (if detail (format " (%s)" detail) "")))

(defun opencode-session--require-session-id ()
  "Return the session ID of the current buffer, or signal an error."
  (or (and opencode-session--session
           (opencode-session-id opencode-session--session))
      (error "OpenCode session is not connected")))

(defun opencode-session--post-model (model-id provider-id variant)
  "Ask the server to switch this session to MODEL-ID from PROVIDER-ID.

VARIANT is omitted when nil.  The accepted selection is recorded locally
so the header updates immediately; the server's `session.model.selected'
event confirms it."
  (let* ((ref (opencode-client--model-ref model-id provider-id variant))
         (connection opencode-session--connection)
         (buffer (current-buffer))
         (session-id (opencode-session--require-session-id)))
    (unless connection
      (error "OpenCode session is not connected"))
    (opencode-client-session-set-model
     connection session-id ref
     :success (lambda (&rest _args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (opencode-session--set-server-model ref)
                    (opencode-session--render-header))))
     :error (lambda (&rest args)
              (message "OpenCode: failed to set model%s"
                       (opencode-session--failure-detail args))))))

(defun opencode-session--post-agent (agent)
  "Ask the server to switch this session to AGENT.

The agent menu cursor is moved immediately and corrected by the
server's `session.agent.selected' event."
  (let ((connection opencode-session--connection)
        (buffer (current-buffer))
        (session-id (opencode-session--require-session-id)))
    (unless connection
      (error "OpenCode session is not connected"))
    (opencode-client-session-set-agent
     connection session-id agent
     :success (lambda (&rest _args)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (opencode-session--set-server-agent agent)
                    (opencode-session--render-header))))
     :error (lambda (&rest args)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (opencode-session--sync-agent-index)))
              (message "OpenCode: failed to set agent%s"
                       (opencode-session--failure-detail args))))))

;;; Agent management

(defun opencode-session--normalize-agent-data (data)
  "Normalize raw agent DATA into a list of alists.
Each element is an alist with at least `name' (or `id') and `mode' keys.
Accepts a bare list/vector as well as a `{data: [...]}` envelope."
  (let ((agents (cond
                 ((vectorp data) (append data nil))
                 ((and (listp data)
                       (let ((cell (assoc 'data data)))
                         (and cell (listp (cdr cell)))))
                  (cdr (assoc 'data data)))
                 ((listp data) data)
                 (t nil))))
    (cl-remove-if-not (lambda (agent)
                        (or (stringp agent) (listp agent)))
                      agents)))

(defun opencode-session--agent-name (agent)
  "Return the name string for AGENT.
AGENT may be a string or an alist."
  (cond
   ((stringp agent) agent)
   ((listp agent) (or (alist-get 'id agent)
                      (alist-get 'name agent)))
   (t nil)))

(defun opencode-session--normalize-agents (data)
  "Normalize agent list DATA into a list of primary agent names."
  (let* ((agents (opencode-session--normalize-agent-data data))
         (primary (cl-remove-if-not (lambda (agent)
                                      (or (stringp agent)
                                          (and (string= (alist-get 'mode agent) "primary")
                                               (not (alist-get 'hidden agent)))))
                                    agents))
         (names (mapcar #'opencode-session--agent-name primary)))
    (delq nil names)))

(defun opencode-session--completable-agent-names (data)
  "Return a list of agent names suitable for @-mention completion from DATA.
Includes non-hidden agents that are not in primary mode (i.e., subagents)."
  (let* ((agents (opencode-session--normalize-agent-data data))
         (completable (cl-remove-if-not
                       (lambda (agent)
                         (and (listp agent)
                              (not (alist-get 'hidden agent))
                              (not (string= (alist-get 'mode agent) "primary"))))
                       agents))
         (names (mapcar #'opencode-session--agent-name completable)))
    (delq nil names)))

(defun opencode-session--maybe-fetch-agents (connection)
  "Fetch and cache agents for CONNECTION when needed."
  (unless (opencode-connection-agents connection)
    (setf (opencode-connection-agents connection) :loading)
    (let ((session-buffer (current-buffer)))
      (opencode-client-agents
       connection
       :success (lambda (&rest args)
                  (let* ((data (plist-get args :data))
                         (raw (opencode-session--normalize-agent-data data))
                         (agents (opencode-session--normalize-agents data)))
                    (setf (opencode-connection-agents connection)
                          (or agents :unavailable))
                    (setf (opencode-connection-agents-raw connection) raw)
                    (when (buffer-live-p session-buffer)
                      (with-current-buffer session-buffer
                        (opencode-session--sync-agent-index)))
                    (opencode-session--refresh-headers connection)))
       :error (lambda (&rest _args)
                (setf (opencode-connection-agents connection) :unavailable)
                (message "OpenCode: failed to load agents"))))))

(defun opencode-session--ensure-agents (connection)
  "Ensure agent list is available for CONNECTION."
  (if (opencode-connection-agents connection)
      (when (eq connection opencode-session--connection)
        (opencode-session--sync-agent-index)
        (opencode-session--render-header))
    (opencode-session--maybe-fetch-agents connection)))

(defun opencode-session--refresh-agents (connection)
  "Refresh the cached agent list for CONNECTION."
  (setf (opencode-connection-agents connection) nil)
  (setf (opencode-connection-agents-raw connection) nil)
  (opencode-session--maybe-fetch-agents connection))

(defun opencode-session--agents-changed (connection)
  "Refresh cached agents for CONNECTION after a catalog change.
Called on the server's `agent.updated' event.  Skips when a fetch is
already in flight."
  (when (and connection
             (not (eq (opencode-connection-agents connection) :loading)))
    (opencode-session--refresh-agents connection)))

(defun opencode-session--available-agents ()
  "Return available agents for the current session buffer."
  (when opencode-session--connection
    (let ((agents (opencode-connection-agents opencode-session--connection)))
      (when (listp agents)
        agents))))

(defun opencode-session--available-completable-agents ()
  "Return agent names available for @-mention completion.
These are non-hidden, non-primary agents (subagents)."
  (when opencode-session--connection
    (let ((raw (opencode-connection-agents-raw opencode-session--connection)))
      (when raw
        (opencode-session--completable-agent-names raw)))))

(defun opencode-session--set-agent (agent index)
  "Ask the server to switch the current session to AGENT.

INDEX is the position of AGENT in the available agents list.  It is
kept as the menu cursor until the server confirms the selection."
  (setq-local opencode-session--agent-index index)
  (opencode-session--post-agent agent)
  (opencode-session--render-header)
  (message "OpenCode agent: %s" agent))

(defun opencode-session-select-agent ()
  "Select an agent for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-agents connection)
           (opencode-session--select-when-loaded
            buffer
            #'opencode-session--available-agents
            (lambda ()
              (unless (eq (opencode-connection-agents connection) :loading)
                (opencode-session--refresh-agents connection)))
            #'opencode-session--prompt-agent-selection
            "agents")))))))

(defun opencode-session--prompt-agent-selection ()
  "Prompt for an agent choice in the current session buffer."
  (let ((agents (opencode-session--available-agents)))
    (unless agents
      (error "OpenCode agents not available"))
    (let* ((agent (completing-read "OpenCode agent: " agents nil t
                                   (or (opencode-session--current-agent)
                                       (car agents))))
           (index (cl-position agent agents :test #'string=)))
      (if (and index agents)
          (opencode-session--set-agent agent index)
        (message "OpenCode: unknown agent %s" agent)))))

(defun opencode-session--cycle-agent (step)
  "Cycle the current agent by STEP positions."
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-agents connection)
           (let ((agents (opencode-session--available-agents)))
             (unless agents
               (error "OpenCode agents not available"))
             (let* ((count (length agents))
                    (current (or opencode-session--agent-index 0))
                    (next (mod (+ current step) count)))
               (opencode-session--set-agent (nth next agents) next)))))))))

(defun opencode-session-next-agent ()
  "Select the next available agent."
  (interactive)
  (opencode-session--cycle-agent 1))

(defun opencode-session-previous-agent ()
  "Select the previous available agent."
  (interactive)
  (opencode-session--cycle-agent -1))

(defun opencode-session-refresh-agents ()
  "Refresh the available agents list for the session."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--refresh-agents connection)))))))

;;; Provider and model selection

(defun opencode-session--ensure-providers (connection)
  "Ensure provider list is available for CONNECTION."
  (when connection
    (let ((providers (opencode-connection-providers connection)))
      (unless (or (and providers (or (vectorp providers) (listp providers)))
                  (memq providers '(:loading :unavailable)))
        (opencode-connection-ensure-providers
         connection
         (lambda (_items)
           (opencode-session--refresh-headers connection)))))))

(defun opencode-session--provider-catalog (connection)
  "Return provider catalog payload for CONNECTION."
  (when connection
    (let ((catalog (opencode-connection-provider-catalog connection)))
      (unless (memq catalog '(:loading :unavailable))
        catalog))))

(defun opencode-session--connected-provider-ids (connection)
  "Return a list of connected provider IDs for CONNECTION."
  (let* ((catalog (opencode-session--provider-catalog connection))
         (connected (and catalog (alist-get 'connected catalog))))
    (cl-remove-if-not #'stringp (opencode-session--normalize-items connected))))

(defun opencode-session--provider-model-items (provider)
  "Return provider model entries from PROVIDER."
  (let ((models (alist-get 'models provider)))
    (cond
     ((hash-table-p models)
      (let (items)
        (maphash (lambda (model-id model-info)
                   (push (cons model-id model-info) items))
                 models)
        (nreverse items)))
     ((listp models)
      (cl-remove-if-not #'consp models))
     (t nil))))

(defun opencode-session--provider-model-info (provider-id model-id &optional connection)
  "Return model metadata for PROVIDER-ID and MODEL-ID from CONNECTION."
  (let* ((conn (or connection opencode-session--connection))
         (catalog (opencode-session--provider-catalog conn))
         (providers (or (opencode-session--normalize-items (and catalog (alist-get 'all catalog)))
                        (opencode-session--normalize-items (and conn (opencode-connection-providers conn)))))
         (provider (and (stringp provider-id)
                        (cl-find provider-id providers
                                 :key (lambda (item) (alist-get 'id item))
                                 :test #'string=))))
    (when provider
      (cdr (cl-assoc model-id (opencode-session--provider-model-items provider)
                     :test #'string=)))))

(defun opencode-session--provider-model-candidate-display (provider-id model-id connected-p)
  "Return completion display text for PROVIDER-ID and MODEL-ID.

CONNECTED-P indicates whether PROVIDER-ID is already connected."
  (format "%s/%s%s"
          provider-id
          model-id
          (if connected-p " (connected)" "")))

(defun opencode-session--provider-model-candidates (&optional connection)
  "Return provider/model completion candidates for CONNECTION.

Each candidate is a plist with provider/model IDs and display text."
  (let* ((conn (or connection opencode-session--connection))
         (catalog (opencode-session--provider-catalog conn))
         (providers (or (opencode-session--normalize-items (and catalog (alist-get 'all catalog)))
                        (opencode-session--normalize-items (and conn (opencode-connection-providers conn)))))
         (connected (opencode-session--connected-provider-ids conn))
         entries)
    (dolist (provider providers)
      (let* ((provider-id (alist-get 'id provider))
             (provider-name (or (alist-get 'name provider) provider-id))
             (connected-p (and (stringp provider-id)
                               (member provider-id connected))))
        (when (stringp provider-id)
          (dolist (entry (opencode-session--provider-model-items provider))
            (let* ((model-id-raw (car entry))
                   (model-id (cond
                              ((stringp model-id-raw) model-id-raw)
                              ((symbolp model-id-raw) (symbol-name model-id-raw))
                              (t nil)))
                   (model-info (cdr entry))
                   (model-name (or (alist-get 'name model-info) model-id))
                   (status (alist-get 'status model-info)))
              (when (and (stringp model-id)
                         (not (string= status "deprecated")))
                (push (list :provider-id provider-id
                            :provider-name provider-name
                            :model-id model-id
                            :model-name model-name
                            :connected-p connected-p
                            :display (opencode-session--provider-model-candidate-display
                                      provider-id
                                      model-id
                                      connected-p))
                      entries)))))))
    (opencode-session--sort-model-candidates entries)))

(defun opencode-session--model-candidate-tier (candidate favorites recent-models session-models)
  "Return the sort tier for CANDIDATE.

FAVORITES is the global favorite list, RECENT-MODELS the global
recently-selected list, and SESSION-MODELS the list of models used in
the current session.  Tier 0 = favorite, 1 = recently selected,
2 = session-used, 3 = connected, 4 = other."
  (let ((key (cons (plist-get candidate :provider-id)
                   (plist-get candidate :model-id))))
    (cond
     ((member key favorites) 0)
     ((member key recent-models) 1)
     ((member key session-models) 2)
     ((plist-get candidate :connected-p) 3)
     (t 4))))

(defun opencode-session--model-candidate-rank (candidate tier ranked-list)
  "Return positional rank for CANDIDATE within TIER.

RANKED-LIST is the ordered list for tiers 0 through 2."
  (if (<= tier 2)
      (let ((key (cons (plist-get candidate :provider-id)
                       (plist-get candidate :model-id))))
        (or (cl-position key ranked-list :test #'equal) 0))
    0))

(defun opencode-session--sort-model-candidates (entries)
  "Sort ENTRIES by tier: favorite, recent, session-used, connected, other."
  (opencode-session--ensure-preferences)
  (let ((favorites opencode-session--favorite-models)
        (recent opencode-session--recent-models)
        (session (opencode-session--session-used-models)))
    (sort entries
          (lambda (a b)
            (let* ((a-tier (opencode-session--model-candidate-tier
                            a favorites recent session))
                   (b-tier (opencode-session--model-candidate-tier
                            b favorites recent session))
                   (rank-list (lambda (tier)
                                (cond ((= tier 0) favorites)
                                      ((= tier 1) recent)
                                      (t session))))
                   (a-rank (opencode-session--model-candidate-rank
                            a a-tier (funcall rank-list a-tier)))
                   (b-rank (opencode-session--model-candidate-rank
                            b b-tier (funcall rank-list b-tier))))
              (cond
               ((< a-tier b-tier) t)
               ((> a-tier b-tier) nil)
               ((/= a-tier b-tier) nil)
               ;; Within tiers 0 through 2, sort by positional rank
               ((<= a-tier 2)
                (< a-rank b-rank))
               ;; Within tiers 2 and 3, sort alphabetically
               (t
                (let ((a-provider (downcase (or (plist-get a :provider-name) "")))
                      (b-provider (downcase (or (plist-get b :provider-name) "")))
                      (a-model (downcase (or (plist-get a :model-name) "")))
                      (b-model (downcase (or (plist-get b :model-name) ""))))
                  (if (string= a-provider b-provider)
                      (string-lessp a-model b-model)
                    (string-lessp a-provider b-provider))))))))))

(defun opencode-session--provider-model-completion-data (&optional connection)
  "Return provider/model completion data for CONNECTION.

The return value is a cons of (CHOICES . LOOKUP)."
  (let ((lookup (make-hash-table :test #'equal))
        choices)
    (dolist (candidate (opencode-session--provider-model-candidates connection))
      (let ((display (plist-get candidate :display)))
        (when (and (stringp display)
                    (not (gethash display lookup)))
          (push display choices)
          (puthash display candidate lookup))))
    (cons (nreverse choices) lookup)))

(defun opencode-session--refresh-headers (connection)
  "Re-render headers for buffers using CONNECTION."
  (maphash
   (lambda (_session-id buffer)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq opencode-session--connection connection)
            (opencode-session--sync-agent-index)
            (opencode-session--sync-variant-selection)
            (opencode-session--render-header)))))
   opencode-session--buffers))

(defun opencode-session--variant-for-model (provider-id model-id)
  "Return the variant to use when selecting MODEL-ID from PROVIDER-ID.

Prefers the session's active variant, then the persisted per-model
variant from the shared preference file.  Returns nil when neither is
set or the model offers no such variant."
  (let ((variant (or (opencode-session--current-variant)
                     (opencode-session--remembered-variant
                      provider-id model-id))))
    (when (and variant
               (cl-member variant (opencode-session--model-variants
                                   provider-id model-id)
                          :test #'string=))
      variant)))

(defun opencode-session--apply-model-selection (provider-id model-id)
  "Select MODEL-ID from PROVIDER-ID as the session model.

Records the choice in the shared recent models list and asks the
server to apply it; the server records the selection and confirms it
with a `session.model.selected' event."
  (opencode-session--record-recent provider-id model-id)
  (opencode-session--post-model
   model-id provider-id
   (opencode-session--variant-for-model provider-id model-id))
  (opencode-session--render-header)
  (message "OpenCode model: %s/%s" provider-id model-id))

;;;###autoload
(defun opencode-session-toggle-model-favorite ()
  "Toggle the active model as a favorite.

Favorites persist in the TUI's shared `model.json' preference file and
sort first in the model selector, matching the TUI's `/models' dialog."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (let ((model (opencode-session--current-model)))
    (unless model
      (error "Select a model first"))
    (opencode-session--ensure-preferences)
    (if (member model opencode-session--favorite-models)
        (progn
          (setq opencode-session--favorite-models
                (cl-remove model opencode-session--favorite-models :test #'equal))
          (opencode-session--save-preferences)
          (message "OpenCode: %s/%s removed from favorites"
                   (car model) (cdr model)))
      (push model opencode-session--favorite-models)
      (opencode-session--save-preferences)
      (message "OpenCode: %s/%s added to favorites"
               (car model) (cdr model)))))

(defun opencode-session--wait-for-load (ready-p on-ready &optional label)
  "Poll READY-P until non-nil, then call ON-READY with no arguments.
LABEL names the resource in the timeout message.  Gives up after about
10 seconds so a stalled fetch cannot poll forever."
  (let ((timer nil)
        (attempts 0))
    (setq timer
          (run-at-time
           0 0.5
           (lambda ()
             (cond
              ((funcall ready-p)
               (cancel-timer timer)
               (funcall on-ready))
              ((>= (cl-incf attempts) 20)
               (cancel-timer timer)
               (message "OpenCode: timed out waiting for %s"
                        (or label "data")))))))))

(defun opencode-session--select-when-loaded (buffer ready-p refresh prompt label)
  "Run PROMPT in BUFFER once READY-P returns non-nil.
REFRESH fetches data when nothing is in flight yet.  PROMPT runs
immediately when READY-P already passes, otherwise after a bounded
wait.  READY-P and PROMPT run in BUFFER.  LABEL names the resource
in status messages."
  (with-current-buffer buffer
    (if (funcall ready-p)
        (funcall prompt)
      (funcall refresh)
      (message "OpenCode: loading %s..." label)
      (opencode-session--wait-for-load
       (lambda ()
         (and (buffer-live-p buffer)
              (with-current-buffer buffer (funcall ready-p))))
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (funcall prompt))))
       label))))

(defun opencode-session-select-model ()
  "Select a provider and model for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
           (opencode-session--select-when-loaded
            buffer
            (lambda ()
              (car (opencode-session--provider-model-completion-data)))
            (lambda ()
              (unless (eq (opencode-connection-providers connection) :loading)
                (opencode-session--refresh-providers connection)))
            (lambda ()
              (opencode-session--prompt-model-selection buffer))
            "providers")))))))

(defun opencode-session--prompt-model-selection (buffer)
  "Prompt for a provider/model choice in BUFFER.
BUFFER must be a live session buffer with loaded provider data."
  (let ((data (opencode-session--provider-model-completion-data)))
    (unless (car data)
      (error "OpenCode providers not available"))
    (let* ((choices (car data))
           (lookup (cdr data))
           (completion-extra-properties
            '(:display-sort-function identity :cycle-sort-function identity))
           (selection (completing-read "OpenCode model: " choices nil t))
           (candidate (gethash selection lookup)))
      (unless candidate
        (error "OpenCode: unknown model selection"))
      (let ((provider-id (plist-get candidate :provider-id))
            (model-id (plist-get candidate :model-id))
            (connected-p (plist-get candidate :connected-p)))
        (if connected-p
            (opencode-session--apply-model-selection provider-id model-id)
          (opencode-session--connect-provider
           provider-id
           (lambda (&rest _ignored)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-session--apply-model-selection
                  provider-id model-id))))))))))

(defalias 'opencode-session-connect-provider #'opencode-session-select-model
  "Select a provider and model for the current session buffer.")

(defun opencode-session--refresh-providers (connection &optional on-success)
  "Force refresh the provider cache for CONNECTION.

ON-SUCCESS is called when providers are loaded."
  (setf (opencode-connection-providers connection) nil)
  (setf (opencode-connection-provider-catalog connection) nil)
  (opencode-connection-ensure-providers
   connection
   (lambda (items)
     (opencode-session--refresh-headers connection)
     (when on-success
       (funcall on-success items)))))

;;; Variant selection

(defun opencode-session--variant-keys (variants)
  "Return variant names from VARIANTS metadata."
  (let (keys)
    (cond
     ((vectorp variants)
      (dolist (entry (append variants nil))
        (when (listp entry)
          (let ((name (alist-get 'id entry)))
            (when (stringp name)
              (push name keys))))))
     ((hash-table-p variants)
      (maphash
       (lambda (key value)
         (let ((name (cond
                      ((stringp key) key)
                      ((symbolp key) (symbol-name key))
                      (t nil))))
           (unless (or (null name)
                       (and (listp value)
                            (eq (alist-get 'disabled value) t)))
             (push name keys))))
       variants))
     ((listp variants)
      (dolist (entry variants)
        (when (consp entry)
          (let* ((key (car entry))
                 (value (cdr entry))
                 (name (cond
                        ((stringp key) key)
                        ((symbolp key) (symbol-name key))
                        ;; v2 decodes variants as a list of info objects.
                        ((stringp (alist-get 'id entry))
                         (alist-get 'id entry))
                        (t nil))))
            (unless (or (null name)
                        (and (listp value)
                             (eq (alist-get 'disabled value) t)))
              (push name keys)))))))
    (sort (delete-dups keys) #'string-lessp)))

(defun opencode-session--model-variants (provider-id model-id)
  "Return variant names available for MODEL-ID from PROVIDER-ID."
  (when-let* ((model-info (opencode-session--provider-model-info
                            provider-id model-id
                            opencode-session--connection)))
    (opencode-session--variant-keys (alist-get 'variants model-info))))

(defun opencode-session--available-variants ()
  "Return available variant names for the active model."
  (when-let* ((model (opencode-session--current-model)))
    (opencode-session--model-variants (car model) (cdr model))))

(defun opencode-session--sync-variant-selection ()
  "Align the variant menu cursor with the server-selected variant."
  (let* ((variants (opencode-session--available-variants))
         (variant (opencode-session--current-variant))
         (index (and variant variants
                     (cl-position variant variants :test #'string=))))
    (setq-local opencode-session--variant-index index)))

(defun opencode-session--set-variant (variant index)
  "Select model VARIANT at INDEX for the current session buffer.

Asks the server to apply VARIANT to the active model; passing nil
clears the variant.  INDEX is kept as the menu cursor until the server
confirms the selection."
  (let ((model (opencode-session--current-model)))
    (unless model
      (error "Select a model first"))
    (setq-local opencode-session--variant-index (and variant index))
    (opencode-session--record-variant (car model) (cdr model) variant)
    (opencode-session--post-model (cdr model) (car model) variant)
    (opencode-session--render-header)
    (message "OpenCode variant: %s" (or variant "none"))))

(defconst opencode-session--no-variant-label "none"
  "Completion label representing no active model variant.")

(defun opencode-session-clear-variant ()
  "Clear the active model variant for the current session buffer."
  (interactive)
  (unless (derived-mode-p 'opencode-session-mode)
    (error "Not in an OpenCode session buffer"))
  (opencode-session--set-variant nil nil))

(defun opencode-session-select-variant ()
  "Select a model variant for the current session buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
           (unless (opencode-session--current-model)
             (error "Select a model first"))
           (let* ((variants (or (opencode-session--available-variants) nil))
                  (choices (cons opencode-session--no-variant-label variants))
                  (initial (or (opencode-session--current-variant)
                               opencode-session-default-variant
                               opencode-session--no-variant-label))
                  (variant (completing-read "OpenCode variant: " choices nil t nil nil initial)))
             (if (or (null variant)
                     (string= variant opencode-session--no-variant-label))
                 (opencode-session-clear-variant)
               (let* ((available (opencode-session--available-variants))
                      (index (and available
                                  (cl-position variant available :test #'string=))))
                 (if (and available index)
                     (opencode-session--set-variant variant index)
                   (message "OpenCode: unknown variant %s" variant)))))))))))

(defun opencode-session--cycle-variant (step)
  "Cycle the current model variant by STEP positions."
  (let ((buffer (current-buffer)))
    (opencode-session--ensure-connection
     (lambda (connection)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (opencode-session--ensure-providers connection)
           (unless (opencode-session--current-model)
             (error "Select a model first"))
           (let* ((variants (or (opencode-session--available-variants) nil))
                  (cycle-values (cons nil variants))
                  (count (length cycle-values))
                  (current (or (and (opencode-session--current-variant)
                                    (let ((index (cl-position (opencode-session--current-variant)
                                                               variants
                                                               :test #'string=)))
                                      (and index (1+ index))))
                               0))
                  (next (mod (+ current step) count))
                  (next-variant (nth next cycle-values)))
             (if next-variant
                 (opencode-session--set-variant next-variant (1- next))
               (opencode-session-clear-variant)))))))))

(defun opencode-session-next-variant ()
  "Select the next variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant 1))

(defun opencode-session-previous-variant ()
  "Select the previous variant (including none) for the current model."
  (interactive)
  (opencode-session--cycle-variant -1))

;;; Auth flows

(defun opencode-session--post-connect-refresh (connection callback)
  "Refresh providers for CONNECTION, then call CALLBACK.
CALLBACK is passed through to `opencode-session--refresh-providers'."
  (opencode-session--refresh-providers connection callback))

(defun opencode-session--connect-provider (provider-id callback)
  "Run the auth flow for PROVIDER-ID, then call CALLBACK on success."
  (let ((connection opencode-session--connection))
    (unless connection
      (error "OpenCode session is not connected"))
    (message "OpenCode: fetching integrations for %s..." provider-id)
    (opencode-client-integrations
     connection
     :success (lambda (&rest args)
                (let* ((data (plist-get args :data))
                       (methods (opencode-session--integration-methods
                                 provider-id data)))
                  (opencode-session--run-auth-flow
                   connection provider-id methods callback)))
     :error (lambda (&rest _args)
              (error "OpenCode: failed to fetch integrations")))))

(defun opencode-session--integration-methods (provider-id data)
  "Return auth methods for PROVIDER-ID from integration DATA.
DATA is the `{location, data: [...]}` envelope of `integration.list'.
Falls back to a single API key method when none are found."
  (let* ((items (opencode-session--normalize-items (alist-get 'data data)))
         (info (cl-find provider-id items
                        :key (lambda (item) (alist-get 'id item))
                        :test #'string=))
         (methods (and info (opencode-session--normalize-items
                             (alist-get 'methods info)))))
    (or methods '(((type . "key") (label . "API key"))))))

(defun opencode-session--auth-method-display (method)
  "Return selection text for auth METHOD."
  (let ((type (alist-get 'type method))
        (label (alist-get 'label method)))
    (if (and (stringp label) (not (string-empty-p label)))
        (format "%s (%s)" label type)
      (or type "unknown"))))

(defun opencode-session--run-auth-flow (connection provider-id methods callback)
  "Run auth for PROVIDER-ID on CONNECTION using METHODS, then CALLBACK."
  (let* ((method (if (= (length methods) 1)
                     (car methods)
                   (opencode-session--choose-auth-method methods)))
         (method-type (alist-get 'type method)))
    (cond
     ((string= method-type "key")
      (opencode-session--auth-api-key connection provider-id method callback))
     ((string= method-type "oauth")
      (opencode-session--auth-oauth connection provider-id method callback))
     ((string= method-type "env")
      (message "OpenCode: %s authenticates via environment variables" provider-id))
     (t (error "OpenCode: unsupported auth method type %s" method-type)))))

(defun opencode-session--choose-auth-method (methods)
  "Prompt the user to choose from METHODS."
  (let* ((displays (mapcar #'opencode-session--auth-method-display methods))
         (completion-extra-properties
          '(:display-sort-function identity :cycle-sort-function identity))
         (selection (completing-read "OpenCode auth method: " displays nil t))
         (index (cl-position selection displays :test #'string=)))
    (nth index methods)))

(defun opencode-session--read-method-answer (method)
  "Prompt for METHOD's extra form fields.
Returns an answer alist, or nil when the method needs no answers."
  (let ((fields (opencode-session--normalize-items (alist-get 'form method))))
    (when fields
      (delq nil (mapcar #'opencode-session--form-read-field fields)))))

(defun opencode-session--auth-api-key (connection provider-id method callback)
  "Prompt for an API key for PROVIDER-ID on CONNECTION, then CALLBACK.
METHOD supplies the prompt label and any extra form fields."
  (let* ((label (or (alist-get 'label method) "API key"))
         (key (read-string (format "%s for %s: " label provider-id))))
    (when (string-empty-p key)
      (error "OpenCode: API key cannot be empty"))
    (message "OpenCode: connecting %s..." provider-id)
    (opencode-client-integration-connect-key
     connection
     provider-id
     key
     :answer (opencode-session--read-method-answer method)
     :success (lambda (&rest _args)
                (message "OpenCode: %s connected" provider-id)
                (opencode-session--post-connect-refresh connection callback))
     :error (lambda (&rest _args)
              (message "OpenCode: failed to connect %s" provider-id)))))

(defun opencode-session--auth-oauth (connection provider-id method callback)
  "Run the OAuth flow for METHOD on CONNECTION.
CALLBACK is called on successful authorization."
  (let ((method-id (alist-get 'id method)))
    (unless method-id
      (error "OpenCode: OAuth method is missing ID"))
    (message "OpenCode: starting OAuth for %s..." provider-id)
    (opencode-client-integration-oauth-begin
     connection
     provider-id
     method-id
     :answer (opencode-session--read-method-answer method)
     :success (lambda (&rest args)
                (let ((attempt (alist-get 'data (plist-get args :data))))
                  (opencode-session--auth-oauth-attempt
                   connection provider-id attempt callback)))
     :error (lambda (&rest _args)
              (message "OpenCode: OAuth authorization failed for %s"
                       provider-id)))))

(defun opencode-session--auth-oauth-attempt (connection provider-id attempt callback)
  "Continue OAuth from ATTEMPT details, then CALLBACK.
Opens the authorization URL and dispatches on the attempt mode."
  (let ((attempt-id (alist-get 'attemptID attempt))
        (url (alist-get 'url attempt))
        (mode (alist-get 'mode attempt))
        (instructions (alist-get 'instructions attempt)))
    (unless attempt-id
      (error "OpenCode: OAuth attempt is missing ID"))
    (when (and (stringp instructions) (not (string-empty-p instructions)))
      (message "OpenCode: %s" instructions))
    (when url
      (let ((browse-url-browser-function #'browse-url-default-browser))
        (browse-url url)))
    (cond
     ((string= mode "code")
      (opencode-session--auth-oauth-code
       connection provider-id attempt-id callback))
     ((string= mode "auto")
      (opencode-session--auth-oauth-poll
       connection provider-id attempt-id callback 0))
     (t (error "OpenCode: unknown OAuth mode %s" mode)))))

(defun opencode-session--auth-oauth-code (connection provider-id attempt-id callback)
  "Complete a code-mode OAuth attempt ATTEMPT-ID, then CALLBACK."
  (let ((code (read-string
               (format "Authorization code for %s: " provider-id))))
    (when (string-empty-p code)
      (error "OpenCode: authorization code cannot be empty"))
    (message "OpenCode: completing OAuth for %s..." provider-id)
    (opencode-client-integration-oauth-complete
     connection
     provider-id
     attempt-id
     :code code
     :success (lambda (&rest _args)
                (message "OpenCode: %s connected" provider-id)
                (opencode-session--post-connect-refresh connection callback))
     :error (lambda (&rest _args)
              (message "OpenCode: OAuth callback failed for %s"
                       provider-id)))))

(defun opencode-session--auth-oauth-poll (connection provider-id attempt-id callback count)
  "Poll an auto-mode OAuth attempt ATTEMPT-ID, then CALLBACK.
COUNT tracks polls; polling stops after about two minutes."
  (opencode-client-integration-oauth-status
   connection
   provider-id
   attempt-id
   :success (lambda (&rest args)
              (let* ((status (alist-get 'data (plist-get args :data)))
                     (state (alist-get 'status status)))
                (cond
                 ((string= state "complete")
                  (message "OpenCode: %s connected" provider-id)
                  (opencode-session--post-connect-refresh connection callback))
                 ((string= state "failed")
                  (message "OpenCode: OAuth failed for %s%s"
                           provider-id
                           (let ((detail (alist-get 'message status)))
                             (if (stringp detail)
                                 (format " (%s)" detail)
                               ""))))
                 ((string= state "expired")
                  (message "OpenCode: OAuth attempt expired for %s" provider-id))
                 ((>= count 60)
                  (message "OpenCode: timed out waiting for OAuth for %s"
                           provider-id))
                 (t (run-at-time 2 nil #'opencode-session--auth-oauth-poll
                                 connection provider-id attempt-id callback
                                 (1+ count))))))
   :error (lambda (&rest _args)
            (message "OpenCode: OAuth status check failed for %s"
                     provider-id))))

(provide 'emacs-opencode-session-model)

;;; emacs-opencode-session-model.el ends here
