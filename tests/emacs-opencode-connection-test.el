;;; emacs-opencode-connection-test.el --- Tests for connection management  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-connection)

;;; base-url

(ert-deftest test-opencode-connection/base-url ()
  "Build a base URL from hostname and port."
  (should (equal (opencode-connection--base-url "127.0.0.1" 4096)
                 "http://127.0.0.1:4096")))

(ert-deftest test-opencode-connection/base-url-custom ()
  "Build a base URL with custom hostname."
  (should (equal (opencode-connection--base-url "example.com" 8080)
                 "http://example.com:8080")))

;;; process-environment

(ert-deftest test-opencode-connection/process-environment-set ()
  "Set an environment variable."
  (let ((process-environment '("HOME=/home/user" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . "bar")))))
      (should (member "FOO=bar" result))
      (should (member "HOME=/home/user" result)))))

(ert-deftest test-opencode-connection/process-environment-override ()
  "Override an existing environment variable."
  (let ((process-environment '("FOO=old" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . "new")))))
      (should (member "FOO=new" result))
      (should-not (member "FOO=old" result)))))

(ert-deftest test-opencode-connection/process-environment-unset ()
  "Unset an environment variable."
  (let ((process-environment '("FOO=bar" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment
                   '(("FOO" . nil)))))
      (should-not (cl-find-if (lambda (item) (string-prefix-p "FOO=" item))
                              result))
      (should (member "PATH=/usr/bin" result)))))

(ert-deftest test-opencode-connection/process-environment-empty ()
  "Empty environment alist returns a copy of process-environment."
  (let ((process-environment '("HOME=/home" "PATH=/usr/bin")))
    (let ((result (opencode-connection--process-environment nil)))
      (should (equal result process-environment))
      ;; Should be a copy, not the same object
      (should-not (eq result process-environment)))))

;;; connection struct

(ert-deftest test-opencode-connection/create-struct ()
  "Create a connection struct with fields."
  (let ((conn (opencode-connection-create
               :base-url "http://localhost:4096"
               :hostname "localhost"
               :port 4096
               :directory "/tmp/")))
    (should (opencode-connection-p conn))
    (should (equal (opencode-connection-base-url conn) "http://localhost:4096"))
    (should (equal (opencode-connection-hostname conn) "localhost"))
    (should (= (opencode-connection-port conn) 4096))
    (should (equal (opencode-connection-directory conn) "/tmp/"))))

;;; alive-p

(ert-deftest test-opencode-connection/alive-p-no-process ()
  "Return nil when no process exists."
  (let ((conn (opencode-connection-create)))
    (should (null (opencode-connection-alive-p conn)))))

;;; maybe-ready

(ert-deftest test-opencode-connection/maybe-ready-matches ()
  "Call ready callback and store the password from server output."
  (let* ((conn (opencode-connection-create))
         (called nil)
         (fake-process (start-process "test-proc" nil "true")))
    (unwind-protect
        (progn
          ;; Stub out the provider/command fetches that fire on ready
          (cl-letf (((symbol-function 'opencode-connection-ensure-providers)
                     (lambda (&rest _) nil))
                    ((symbol-function 'opencode-connection-ensure-commands)
                     (lambda (&rest _) nil)))
            (opencode-connection--maybe-ready
             fake-process
             "some output server password PMfwPIGD7ezehQ4kSqePMtqmLVthY6zlOYFmoZxfRHs"
             conn
             (lambda (_proc) (setq called t))))
          (should called)
          (should (equal (opencode-connection-password conn)
                         "PMfwPIGD7ezehQ4kSqePMtqmLVthY6zlOYFmoZxfRHs")))
      (when (process-live-p fake-process)
        (delete-process fake-process)))))

(ert-deftest test-opencode-connection/maybe-ready-no-match ()
  "Don't call callback when output doesn't contain ready string."
  (let* ((conn (opencode-connection-create))
         (called nil)
         (fake-process (start-process "test-proc" nil "true")))
    (unwind-protect
        (progn
          (opencode-connection--maybe-ready
           fake-process
           "some other output"
           conn
           (lambda (_proc) (setq called t)))
          (should (null called)))
      (when (process-live-p fake-process)
        (delete-process fake-process)))))

;;; provider payloads (v2)

(ert-deftest test-opencode-connection/provider-items-v2-envelope ()
  "Unwrap the v2 {data: [...]} provider envelope."
  (should (equal (opencode-connection--provider-items
                  '((data . (((id . "anthropic"))))))
                 '(((id . "anthropic"))))))

(ert-deftest test-opencode-connection/provider-items-v1-shape ()
  "Accept the legacy {all: [...]} provider shape."
  (should (equal (opencode-connection--provider-items
                  '((all . (((id . "anthropic"))))))
                 '(((id . "anthropic"))))))

(ert-deftest test-opencode-connection/provider-items-vector ()
  "Normalize a vector payload to a list."
  (should (equal (opencode-connection--provider-items
                  '((data . [((id . "a"))])))
                 '(((id . "a"))))))

(ert-deftest test-opencode-connection/ensure-providers-caches-v2 ()
  "A v2 provider fetch attaches models and does not refire."
  (let ((conn (opencode-connection-create))
        (provider-calls 0)
        (model-calls 0)
        (providers '((data . (((id . "anthropic") (name . "Anthropic"))))))
        (models '((data . (((providerID . "anthropic")
                            (modelID . "claude")
                            (enabled . t)))))))
    (cl-letf (((symbol-function 'opencode-client-providers)
               (lambda (_conn &rest args)
                 (setq provider-calls (1+ provider-calls))
                 (funcall (plist-get args :success) :data providers)))
              ((symbol-function 'opencode-client-models)
               (lambda (_conn &rest args)
                 (setq model-calls (1+ model-calls))
                 (funcall (plist-get args :success) :data models))))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= provider-calls 1))
      (should (= model-calls 1))
      ;; Models are attached; catalog synthesizes all/connected.
      (let* ((items (opencode-connection-providers conn))
             (provider (car items))
             (catalog (opencode-connection-provider-catalog conn)))
        (should (equal (cdr (assoc 'models provider))
                       '(("claude" . ((providerID . "anthropic")
                                      (modelID . "claude")
                                      (enabled . t))))))
        (should (equal (alist-get 'connected catalog) '("anthropic"))))
      ;; Second call uses the cache without refiring.
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= provider-calls 1))
      (should (= model-calls 1)))))

(ert-deftest test-opencode-connection/ensure-providers-empty-marks-unavailable ()
  "An empty provider list is cached so callers do not retry."
  (let ((conn (opencode-connection-create))
        (calls 0))
    (cl-letf (((symbol-function 'opencode-client-providers)
               (lambda (_conn &rest args)
                 (setq calls (1+ calls))
                 (funcall (plist-get args :success) :data '((data . [])))))
              ((symbol-function 'opencode-client-models)
               (lambda (_conn &rest _args)
                 (error "models must not be fetched without providers"))))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= calls 1))
      (should (eq (opencode-connection-providers conn) :unavailable))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= calls 1)))))

(ert-deftest test-opencode-connection/ensure-providers-no-timer-retry ()
  "A latched empty marker never refires on its own."
  (let ((conn (opencode-connection-create))
        (calls 0))
    (cl-letf (((symbol-function 'opencode-client-providers)
               (lambda (_conn &rest args)
                 (setq calls (1+ calls))
                 (funcall (plist-get args :success) :data '((data . [])))))
              ((symbol-function 'opencode-client-models)
               (lambda (_conn &rest _args) nil)))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= calls 1))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= calls 1))
      (should (eq (opencode-connection-providers conn) :unavailable)))))

(ert-deftest test-opencode-connection/providers-changed-refetches ()
  "A catalog change clears the empty marker and refetches."
  (let ((conn (opencode-connection-create))
        (calls 0))
    (cl-letf (((symbol-function 'opencode-client-providers)
               (lambda (_conn &rest args)
                 (setq calls (1+ calls))
                 (funcall (plist-get args :success) :data '((data . [])))))
              ((symbol-function 'opencode-client-models)
               (lambda (_conn &rest _args) nil)))
      (opencode-connection-ensure-providers conn #'ignore #'ignore)
      (should (= calls 1))
      (should (eq (opencode-connection-providers conn) :unavailable))
      (opencode-connection-providers-changed conn)
      (should (= calls 2))
      (should (eq (opencode-connection-providers conn) :unavailable)))))

(provide 'emacs-opencode-connection-test)

;;; emacs-opencode-connection-test.el ends here
