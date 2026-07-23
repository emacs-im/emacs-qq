;;; qq-gateway-rpc-test.el --- Tests for typed Gateway RPCs -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-rpc)

(defvar qq-gateway-rpc-test--latest nil)

(ert-deftest qq-gateway-rpc-preflight-errors-are-synchronous ()
  (let (failure transport-called)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () nil))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (&rest _arguments)
                 (setq transport-called t))))
      (should-not
       (qq-gateway-rpc-call
        "account.list" nil
        :errback (lambda (body reason)
                   (setq failure (list body reason)))))
      (should-not transport-called)
      (should (equal (alist-get 'code (car failure)) "gateway_not_ready"))
      (should (stringp (cadr failure))))))

(ert-deftest qq-gateway-rpc-rejects-unadvertised-method-synchronously ()
  (let (failure)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("account.list"))))
      (should-not
       (qq-gateway-rpc-call
        "message.send" nil
        :errback (lambda (body reason)
                   (setq failure (list body reason)))))
      (should (equal (alist-get 'code (car failure))
                     "capability_unavailable")))))

(ert-deftest qq-gateway-rpc-preserves-params-and-transport-token ()
  (let ((params '((account_id . "slot-a") (message . "hello")))
        sent result)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("message.send")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (method actual success _error)
                 (setq sent (list method actual))
                 (funcall success '((value . "wire")))
                 "request-7")))
      (should
       (equal
        (qq-gateway-rpc-call
         "message.send" params
         :projector
         (lambda (result)
           (concat (alist-get 'value result) "-projected"))
         :callback (lambda (value) (setq result value)))
        "request-7"))
      (should (equal sent (list "message.send" params)))
      ;; The RPC boundary must never add a runtime generation to ordinary
      ;; protocol-v2 request parameters.
      (should-not (assq 'generation (cadr sent)))
      (should (equal result "wire-projected")))))

(ert-deftest qq-gateway-rpc-domainizes-and-owns-success-before-projection ()
  (let* ((source (copy-sequence "mutable"))
         (wire `((items . [,source ,qq-gateway-wire-null])))
         projected delivered)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success _error)
                 (funcall success wire)
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :projector
       (lambda (domain)
         (setq projected domain)
         domain)
       :callback (lambda (domain) (setq delivered domain))))
    (let ((items (alist-get 'items projected)))
      (should (listp items))
      (should (equal items (list "mutable" nil)))
      (should-not (eq (car items) source))
      (aset (car items) 0 ?X)
      (should (equal source "mutable")))
    (should (equal delivered '((items . ("mutable" nil)))))))

(ert-deftest qq-gateway-rpc-transformer-errors-become-invalid-result ()
  (dolist (stage '(projector current))
    (let (failure callback-called)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("test.method")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params success _error)
                   (funcall success '((value . t)))
                   "request")))
        (qq-gateway-rpc-call
         "test.method" nil
         :projector (if (eq stage 'projector)
                        (lambda (_result) (error "bad projector"))
                      #'identity)
         :current-p (if (eq stage 'current)
                        (lambda () (error "bad guard"))
                      (lambda () t))
         :callback (lambda (_value) (setq callback-called t))
         :errback (lambda (body reason)
                    (setq failure (list body reason))))
        (should-not callback-called)
        (should (equal (alist-get 'code (car failure))
                       "invalid_gateway_result"))
        (should (string-match-p "bad" (cadr failure)))))))

(ert-deftest qq-gateway-rpc-stale-context-has-distinct-error ()
  (let (failure projected)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success _error)
                 (funcall success nil)
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :current-p (lambda () nil)
       :projector (lambda (_result) (setq projected t))
       :stale-code "superseded_request"
       :stale-message "Directory request was superseded"
       :errback (lambda (body reason)
                  (setq failure (list body reason))))
      (should-not projected)
      (should (equal (alist-get 'code (car failure)) "superseded_request"))
      (should (equal (cadr failure) "Directory request was superseded")))))

(ert-deftest qq-gateway-rpc-stale-context-rejects-late-transport-error ()
  (let (failure)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params _success error)
                 (funcall error
                          '((code . "server_failure")
                            (message . "late failure"))
                          "late failure")
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :current-p (lambda () nil)
       :stale-code "superseded_request"
       :stale-message "Request owner changed"
       :errback (lambda (body reason)
                  (setq failure (list body reason))))
      (should (equal (alist-get 'code (car failure)) "superseded_request"))
      (should (equal (cadr failure) "Request owner changed")))))

(ert-deftest qq-gateway-rpc-current-p-error-on-transport-failure-is-invalid ()
  (let (failure)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params _success error)
                 (funcall error nil "server failure")
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :current-p (lambda () (error "bad failure guard"))
       :errback (lambda (body reason)
                  (setq failure (list body reason))))
      (should (equal (alist-get 'code (car failure))
                     "invalid_gateway_result"))
      (should (string-match-p "bad failure guard" (cadr failure))))))

(ert-deftest qq-gateway-rpc-leaf-errors-are-not-invalid-results ()
  (let (errback-called)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success _error)
                 (funcall success '((value . t)))
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :callback (lambda (_value) (error "consumer bug"))
       :errback (lambda (&rest _arguments) (setq errback-called t)))
      (should-not errback-called))))

(ert-deftest qq-gateway-rpc-copies-values-at-leaf-boundaries ()
  (let* ((wire-value (copy-sequence "value"))
         (wire-body `((code . "failed") (message . ,wire-value))))
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("test.method")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params _success error)
                 (funcall error wire-body "failed")
                 "request")))
      (qq-gateway-rpc-call
       "test.method" nil
       :errback (lambda (body _reason)
                  (aset (alist-get 'message body) 0 ?X))))
    (should (equal wire-value "value"))))

(ert-deftest qq-gateway-rpc-latest-call-settles-leaf-exactly-once ()
  (let (qq-gateway-rpc-test--latest success failure callbacks failures)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("registry.list")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success-callback failure-callback)
                 (setq success success-callback
                       failure failure-callback)
                 'opaque-request-token)))
      (should
       (eq
        (qq-gateway-rpc-latest-call
         'qq-gateway-rpc-test--latest "registry.list" nil
         :projector (lambda (result) (alist-get 'value result))
         :callback (lambda (value) (push value callbacks))
         :errback (lambda (body reason)
                    (push (list body reason) failures)))
        'opaque-request-token))
      (funcall success '((value . "first")))
      (funcall failure
               '((code . "late_error") (message . "late"))
               "late")
      (funcall success '((value . "duplicate")))
      (should (equal callbacks '("first")))
      (should-not failures)
      (should-not qq-gateway-rpc-test--latest))))

(ert-deftest qq-gateway-rpc-latest-callback-can-start-successor ()
  (let (qq-gateway-rpc-test--latest first-success canceled delivered
        (send-count 0))
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("registry.list")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success _failure)
                 (cl-incf send-count)
                 (if (= send-count 1)
                     (progn (setq first-success success) 'first-token)
                   'replacement-token)))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token canceled) t)))
      (should
       (eq
        (qq-gateway-rpc-latest-call
         'qq-gateway-rpc-test--latest "registry.list" nil
         :projector (lambda (result) (alist-get 'value result))
         :callback
         (lambda (value)
           (push value delivered)
           (qq-gateway-rpc-latest-call
            'qq-gateway-rpc-test--latest "registry.list" nil)))
        'first-token))
      (funcall first-success '((value . "first")))
      (should (equal delivered '("first")))
      (should-not canceled)
      (should
       (eq (qq-gateway-rpc-latest-request-transport-token
            qq-gateway-rpc-test--latest)
           'replacement-token))
      (qq-gateway-rpc-cancel-latest 'qq-gateway-rpc-test--latest)
      (should (equal canceled '(replacement-token))))))

(ert-deftest qq-gateway-rpc-latest-call-cleans-owner-on-nonlocal-exit ()
  (dolist (condition '(error quit))
    (let (qq-gateway-rpc-test--latest caught)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("registry.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (&rest _arguments)
                   (signal condition '("transport aborted")))))
        (condition-case error-data
            (qq-gateway-rpc-latest-call
             'qq-gateway-rpc-test--latest
             "registry.list" nil :callback #'ignore)
          (error (setq caught (car error-data)))
          (quit (setq caught 'quit)))
        (should (eq caught condition))
        (should-not qq-gateway-rpc-test--latest)))))

(ert-deftest qq-gateway-rpc-latest-call-cancels-and-settles-predecessor ()
  (let (qq-gateway-rpc-test--latest requests canceled first-errors
        second-errors)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("registry.list")))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params success failure)
                 (setq requests
                       (append requests (list (cons success failure))))
                 (intern (format "owned-token-%d" (length requests)))))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token canceled) t)))
      (qq-gateway-rpc-latest-call
       'qq-gateway-rpc-test--latest "registry.list" nil
       :errback (lambda (body _reason) (push body first-errors)))
      (qq-gateway-rpc-latest-call
       'qq-gateway-rpc-test--latest "registry.list" nil
       :errback (lambda (body _reason) (push body second-errors)))
      (should (equal canceled '(owned-token-1)))
      (should (= (length first-errors) 1))
      (should (equal (alist-get 'code (car first-errors))
                     "superseded_request"))
      (funcall (car (nth 0 requests)) '((ignored . t)))
      (funcall (cdr (nth 0 requests)) nil "late")
      (should (= (length first-errors) 1))
      (qq-gateway-rpc-cancel-latest
       'qq-gateway-rpc-test--latest "superseded_request" "reset")
      (should (equal canceled '(owned-token-2 owned-token-1)))
      (should (= (length second-errors) 1))
      (should-not qq-gateway-rpc-test--latest))))

(provide 'qq-gateway-rpc-test)

;;; qq-gateway-rpc-test.el ends here
