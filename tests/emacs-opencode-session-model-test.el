;;; emacs-opencode-session-model-test.el --- Tests for model selection  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-model)

;;; normalize-agents

(ert-deftest test-opencode-model/normalize-agents-strings ()
  "Normalize a list of string agents."
  (should (equal (opencode-session--normalize-agents '("plan" "code"))
                 '("plan" "code"))))

(ert-deftest test-opencode-model/normalize-agents-vector ()
  "Normalize a vector of agents."
  (should (equal (opencode-session--normalize-agents ["plan" "code"])
                 '("plan" "code"))))

(ert-deftest test-opencode-model/normalize-agents-alists ()
  "Normalize alist agents with mode and hidden."
  (let ((data '(((id . "plan") (mode . "primary") (hidden . nil))
                ((id . "code") (mode . "primary") (hidden . nil))
                ((id . "hidden") (mode . "primary") (hidden . t))
                ((id . "secondary") (mode . "secondary") (hidden . nil)))))
    (let ((result (opencode-session--normalize-agents data)))
      (should (member "plan" result))
      (should (member "code" result))
      (should-not (member "hidden" result))
      (should-not (member "secondary" result)))))

(ert-deftest test-opencode-model/normalize-agents-nil ()
  "Return nil for nil."
  (should (null (opencode-session--normalize-agents nil))))

(ert-deftest test-opencode-model/normalize-agents-uses-name-fallback ()
  "Fall back to name when id is absent."
  (let ((data '(((name . "plan") (mode . "primary")))))
    (should (equal (opencode-session--normalize-agents data) '("plan")))))

(ert-deftest test-opencode-model/normalize-agents-v2-envelope ()
  "Unwrap a v2 {data: [...]} agent envelope."
  (let ((data (list (list (cons 'location "x"))
                    (cons 'data
                          (list (list (cons 'id "plan")
                                      (cons 'mode "primary")))))))
    (should (equal (opencode-session--normalize-agents data) '("plan")))))

(ert-deftest test-opencode-model/maybe-fetch-agents-caches-once ()
  "An agent fetch caches results instead of refiring."
  (let ((conn (opencode-connection-create :directory "/tmp/"))
        (calls 0))
    (cl-letf (((symbol-function 'opencode-client-agents)
               (lambda (_conn &rest args)
                 (setq calls (1+ calls))
                 (funcall (plist-get args :success)
                          :data (list (cons 'data
                                            (list (list (cons 'id "plan")
                                                        (cons 'mode
                                                              "primary")))))))))
      (opencode-session--maybe-fetch-agents conn)
      (should (= calls 1))
      (should (equal (opencode-connection-agents conn) '("plan")))
      (opencode-session--maybe-fetch-agents conn)
      (should (= calls 1)))))

;;; variant-keys

(ert-deftest test-opencode-model/variant-keys-alist ()
  "Extract variant names from alist."
  (let ((result (opencode-session--variant-keys
                 '((fast . nil)
                   (slow . nil)
                   (disabled . ((disabled . t)))))))
    (should (member "fast" result))
    (should (member "slow" result))
    (should-not (member "disabled" result))))

(ert-deftest test-opencode-model/variant-keys-hash-table ()
  "Extract variant names from hash table."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "fast" nil ht)
    (puthash "slow" nil ht)
    (puthash "off" '((disabled . t)) ht)
    (let ((result (opencode-session--variant-keys ht)))
      (should (member "fast" result))
      (should (member "slow" result))
      (should-not (member "off" result)))))

(ert-deftest test-opencode-model/variant-keys-sorted ()
  "Variant keys are sorted alphabetically."
  (let ((result (opencode-session--variant-keys
                 '((zebra . nil) (alpha . nil) (middle . nil)))))
    (should (equal result '("alpha" "middle" "zebra")))))

(ert-deftest test-opencode-model/variant-keys-nil ()
  "Return nil for nil."
  (should (null (opencode-session--variant-keys nil))))

(ert-deftest test-opencode-model/variant-keys-symbol-keys ()
  "Handle symbol keys in alist."
  (let ((result (opencode-session--variant-keys
                 '((fast . nil) (slow . nil)))))
    (should (member "fast" result))
    (should (member "slow" result))))

(ert-deftest test-opencode-model/variant-keys-vector ()
  "Extract v2 variant ids from a vector of info objects."
  (let ((result (opencode-session--variant-keys
                 `[((id . "fast")) ((id . "slow"))])))
    (should (equal result '("fast" "slow")))))

(ert-deftest test-opencode-model/variant-keys-v2-list ()
  "Extract v2 variant ids from a decoded list of info objects."
  (let ((result (opencode-session--variant-keys
                 '(((id . "low") (settings . nil))
                   ((id . "high") (settings . nil))))))
    (should (equal result '("high" "low")))))

;;; model-candidate-tier

(ert-deftest test-opencode-model/candidate-tier-recent ()
  "Recently selected models are tier 0."
  (let ((candidate (list :provider-id "anthropic" :model-id "claude-3" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier
                candidate
                '(("anthropic" . "claude-3"))
                nil)
               0))))

(ert-deftest test-opencode-model/candidate-tier-session ()
  "Session-used models are tier 1."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier
                candidate
                nil
                '(("openai" . "gpt-4")))
               1))))

(ert-deftest test-opencode-model/candidate-tier-connected ()
  "Connected models are tier 2."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p t)))
    (should (= (opencode-session--model-candidate-tier candidate nil nil)
               2))))

(ert-deftest test-opencode-model/candidate-tier-other ()
  "Other models are tier 3."
  (let ((candidate (list :provider-id "openai" :model-id "gpt-4" :connected-p nil)))
    (should (= (opencode-session--model-candidate-tier candidate nil nil)
               3))))

;;; model-candidate-rank

(ert-deftest test-opencode-model/candidate-rank-in-list ()
  "Rank from position in ordered list."
  (let ((candidate (list :provider-id "b" :model-id "2"))
        (ranked '(("a" . "1") ("b" . "2") ("c" . "3"))))
    (should (= (opencode-session--model-candidate-rank candidate 0 ranked) 1))))

(ert-deftest test-opencode-model/candidate-rank-not-in-list ()
  "Default rank 0 when not in list."
  (let ((candidate (list :provider-id "x" :model-id "y")))
    (should (= (opencode-session--model-candidate-rank candidate 0 nil) 0))))

(ert-deftest test-opencode-model/candidate-rank-high-tier ()
  "Tier > 1 always returns rank 0."
  (let ((candidate (list :provider-id "x" :model-id "y")))
    (should (= (opencode-session--model-candidate-rank candidate 2 nil) 0))))

;;; provider-model-items

(ert-deftest test-opencode-model/provider-model-items-list ()
  "Extract model items from a list."
  (let ((provider '((id . "anthropic")
                    (models . (("claude-3" . ((name . "Claude 3")))
                               ("claude-2" . ((name . "Claude 2"))))))))
    (let ((items (opencode-session--provider-model-items provider)))
      (should (= (length items) 2))
      (should (equal (caar items) "claude-3")))))

(ert-deftest test-opencode-model/provider-model-items-hash-table ()
  "Extract model items from a hash table."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "claude-3" '((name . "Claude 3")) ht)
    (let ((provider `((id . "anthropic") (models . ,ht))))
      (let ((items (opencode-session--provider-model-items provider)))
        (should (= (length items) 1))
        (should (equal (caar items) "claude-3"))))))

(ert-deftest test-opencode-model/provider-model-items-nil ()
  "Return nil for nil models."
  (should (null (opencode-session--provider-model-items '((id . "x"))))))

;;; provider-model-candidate-display

(ert-deftest test-opencode-model/candidate-display-connected ()
  "Display connected indicator."
  (should (equal (opencode-session--provider-model-candidate-display
                  "anthropic" "claude-3" t)
                 "anthropic/claude-3 (connected)")))

(ert-deftest test-opencode-model/candidate-display-not-connected ()
  "Display without connected indicator."
  (should (equal (opencode-session--provider-model-candidate-display
                  "anthropic" "claude-3" nil)
                 "anthropic/claude-3")))

;;; completable-agent-names

(ert-deftest test-opencode-model/completable-agent-names-subagents ()
  "Return non-hidden, non-primary agents."
  (let ((data '(((id . "build") (mode . "primary") (hidden . nil))
                ((id . "plan") (mode . "primary") (hidden . nil))
                ((id . "general") (mode . "subagent") (hidden . nil))
                ((id . "explore") (mode . "subagent") (hidden . nil))
                ((id . "compaction") (mode . "primary") (hidden . t))
                ((id . "title") (mode . "primary") (hidden . t)))))
    (let ((result (opencode-session--completable-agent-names data)))
      (should (member "general" result))
      (should (member "explore" result))
      (should-not (member "build" result))
      (should-not (member "plan" result))
      (should-not (member "compaction" result))
      (should-not (member "title" result)))))

(ert-deftest test-opencode-model/completable-agent-names-nil ()
  "Return nil for nil data."
  (should (null (opencode-session--completable-agent-names nil))))

(ert-deftest test-opencode-model/completable-agent-names-vector ()
  "Handle vector input."
  (let ((data [((id . "explore") (mode . "subagent") (hidden . nil))]))
    (should (equal (opencode-session--completable-agent-names data)
                   '("explore")))))

(ert-deftest test-opencode-model/completable-agent-names-all-mode ()
  "Include agents with mode \"all\" that are not hidden."
  (let ((data '(((id . "multi") (mode . "all") (hidden . nil)))))
    (should (equal (opencode-session--completable-agent-names data)
                   '("multi")))))

;;; agent-name

(ert-deftest test-opencode-model/agent-name-string ()
  "Return string agent as-is."
  (should (equal (opencode-session--agent-name "plan") "plan")))

(ert-deftest test-opencode-model/agent-name-alist-with-id ()
  "Extract id from alist agent."
  (should (equal (opencode-session--agent-name '((id . "explore") (name . "Explorer")))
                 "explore")))

(ert-deftest test-opencode-model/agent-name-alist-name-fallback ()
  "Fall back to name when id is absent."
  (should (equal (opencode-session--agent-name '((name . "explore")))
                 "explore")))

(ert-deftest test-opencode-model/agent-name-nil ()
  "Return nil for nil."
  (should (null (opencode-session--agent-name nil))))

;;; integration-methods

(ert-deftest test-opencode-model/integration-methods ()
  "Extract auth methods for a provider from an integration envelope."
  (let* ((method (list (cons 'type "key")))
         (integration (list (cons 'id "anthropic")
                            (cons 'methods (list method))))
         (data (list (cons 'data (list integration)))))
    (let ((result (opencode-session--integration-methods "anthropic" data)))
      (should (= (length result) 1))
      (should (equal (alist-get 'type (car result)) "key")))))
(ert-deftest test-opencode-model/integration-methods-fallback ()
  "Fall back to API key when the provider is unknown."
  (let ((result (opencode-session--integration-methods "unknown" nil)))
    (should (= (length result) 1))
    (should (equal (alist-get 'type (car result)) "key"))))

(ert-deftest test-opencode-model/auth-method-display ()
  "Display falls back to the method type without a label."
  (should (equal (opencode-session--auth-method-display
                  '((type . "key")))
                 "key"))
  (should (equal (opencode-session--auth-method-display
                  '((type . "oauth") (label . "Login")))
                 "Login (oauth)")))

(ert-deftest test-opencode-model/auth-api-key-connects ()
  "The key flow posts the key and refreshes on success."
  (let ((posted nil)
        (refreshed nil))
    (cl-letf (((symbol-function 'opencode-client-integration-connect-key)
               (lambda (_conn _id key &rest args)
                 (setq posted key)
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-session--post-connect-refresh)
               (lambda (_conn callback) (setq refreshed t)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "sk-test")))
      (opencode-session--auth-api-key 'conn "anthropic"
                                      '((type . "key")) #'ignore)
      (should (equal posted "sk-test"))
      (should refreshed))))

(ert-deftest test-opencode-model/auth-oauth-code-completes ()
  "A code-mode attempt completes with the entered code."
  (let ((completed nil))
    (cl-letf (((symbol-function 'opencode-client-integration-oauth-complete)
               (lambda (_conn _id _attempt &rest args)
                 (setq completed (plist-get args :code))
                 (funcall (plist-get args :success))))
              ((symbol-function 'opencode-session--post-connect-refresh)
               (lambda (_conn _callback) nil))
              ((symbol-function 'read-string)
               (lambda (&rest _) "authcode123")))
      (opencode-session--auth-oauth-code 'conn "openai" "con_1" #'ignore)
      (should (equal completed "authcode123")))))

;;; v2 provider flow

(defun opencode-model-test--v2-providers (_conn &rest args)
  "Stub `opencode-client-providers' with a v2 envelope."
  (funcall (plist-get args :success)
           :data (list (cons 'data
                             (list (list (cons 'id "google")
                                         (cons 'name "Google")))))))

(defun opencode-model-test--v2-models (_conn &rest args)
  "Stub `opencode-client-models' with a v2 envelope."
  (funcall (plist-get args :success)
           :data (list (cons 'data
                             (list (list (cons 'providerID "google")
                                         (cons 'modelID "gemini-x")
                                         (cons 'name "Gemini X")
                                         (cons 'enabled t)
                                         (cons 'status "active")
                                         (cons 'variants
                                               (list (list (cons 'id "low"))
                                                     (list (cons 'id "high"))))))))))

(ert-deftest test-opencode-model/v2-flow-builds-candidates ()
  "A v2 provider+model fetch yields selectable candidates."
  (let ((conn (opencode-connection-create :directory "/tmp/")))
    (cl-letf (((symbol-function 'opencode-client-providers)
               #'opencode-model-test--v2-providers)
              ((symbol-function 'opencode-client-models)
               #'opencode-model-test--v2-models)
              ((symbol-function 'opencode-session--session-used-models)
               #'ignore))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (let ((data (opencode-session--provider-model-completion-data conn)))
        (should (member "google/gemini-x (connected)" (car data))))
      (with-temp-buffer
        (setq-local opencode-session--connection conn)
        (setq-local opencode-session--provider-id "google")
        (setq-local opencode-session--model-id "gemini-x")
        (should (equal (opencode-session--available-variants)
                       '("high" "low")))))))

(ert-deftest test-opencode-model/prompt-model-selection-applies ()
  "The extracted model prompt applies a connected selection."
  (let ((conn (opencode-connection-create :directory "/tmp/")))
    (cl-letf (((symbol-function 'opencode-client-providers)
               #'opencode-model-test--v2-providers)
              ((symbol-function 'opencode-client-models)
               #'opencode-model-test--v2-models)
              ((symbol-function 'opencode-session--session-used-models)
               #'ignore)
              ((symbol-function 'completing-read)
               (lambda (&rest _) "google/gemini-x (connected)"))
              ((symbol-function 'opencode-session--render-header)
               #'ignore))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (with-temp-buffer
        (setq-local opencode-session--connection conn)
        (opencode-session--prompt-model-selection (current-buffer))
        (should (equal opencode-session--provider-id "google"))
        (should (equal opencode-session--model-id "gemini-x"))))))

(ert-deftest test-opencode-model/wait-for-load-fires-when-ready ()
  "The load waiter calls back once the readiness check passes."
  (let (fired)
    (opencode-session--wait-for-load (lambda () t)
                                     (lambda () (setq fired t))
                                     "test")
    (sit-for 0.6)
    (should fired)))

(provide 'emacs-opencode-session-model-test)

;;; emacs-opencode-session-model-test.el ends here
