;;; qq-notifications-test.el --- Tests for qq notifications -*- lexical-binding: t; -*-

(require 'ert)
(require 'qq-notifications)

(defun qq-notifications-test--message (&optional mentions)
  "Return one incoming normalized test message with MENTIONS."
  `((id . "9007199254741004991")
    (server-id . "9007199254741004991")
    (session-key . "group:20001")
    (time . ,(truncate (float-time)))
    (sender-id . "10001")
    (sender-name . "Alice")
    (self-p . nil)
    (status . received)
    (preview . "hello")
    (mention-kinds . ,mentions)))

(ert-deftest qq-notifications-muted-chat-requires-native-priority-mention ()
  (cl-letf (((symbol-function 'qq-state-session)
             (lambda (_key)
               '((key . "group:20001") (type . group)
                 (title . "Group") (muted-p . t))))
            ((symbol-function 'qq-notifications--chat-observable-p)
             (lambda (_key) nil)))
    (should-not (qq-notifications-message-notify-p
                 (qq-notifications-test--message)))
    (should (qq-notifications-message-notify-p
             (qq-notifications-test--message '(at-me))))
    (let ((qq-notifications-at-all-breaks-mute t))
      (should (qq-notifications-message-notify-p
               (qq-notifications-test--message '(at-all)))))
    (let ((qq-notifications-at-all-breaks-mute nil))
      (should-not (qq-notifications-message-notify-p
                   (qq-notifications-test--message '(at-all)))))))

(ert-deftest qq-notifications-suppresses-selected-focused-chat ()
  (cl-letf (((symbol-function 'qq-state-session)
             (lambda (_key)
               '((key . "group:20001") (type . group)
                 (title . "Group") (muted-p . nil))))
            ((symbol-function 'qq-notifications--chat-observable-p)
             (lambda (_key) t)))
    (should-not (qq-notifications-message-notify-p
                 (qq-notifications-test--message '(at-me))))))

(ert-deftest qq-notifications-state-hook-dedupes-and-delays-only-live-creates ()
  (let ((qq-notifications--seen-anchors (make-hash-table :test #'equal))
        (qq-notifications--seen-anchor-order nil)
        (qq-notifications-delay 0.5)
        scheduled)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_delay _repeat function message)
                 (push (list function message) scheduled)
                 'timer)))
      (let ((event (list :type 'message
                         :mutation 'create
                         :source 'event
                         :message (qq-notifications-test--message '(at-me)))))
        (qq-notifications--handle-state-change event)
        (qq-notifications--handle-state-change event)
        (should (= 1 (length scheduled))))
      (qq-notifications--handle-state-change
       (list :type 'message :mutation 'update :source 'event
             :message (qq-notifications-test--message '(at-me))))
      (should (= 1 (length scheduled)))
      (qq-notifications--handle-state-change '(:type reset))
      (should (= 0 (hash-table-count qq-notifications--seen-anchors)))
      (should-not qq-notifications--seen-anchor-order))))

(ert-deftest qq-notifications-body-marks-direct-mention ()
  (let ((qq-notifications-body-limit 80)
        (qq-notifications-show-preview t))
    (should (equal "@你  hello"
                   (qq-notifications--body
                    (qq-notifications-test--message '(at-me)))))))

(provide 'qq-notifications-test)

;;; qq-notifications-test.el ends here
