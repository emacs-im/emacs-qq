;;; qq-guild-channel-type.el --- QQ Guild channel capabilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Declarative UI capabilities for the closed channel kinds projected by the
;; Linux QQ Guild directory.  Protocol numbers remain at the NapCat boundary.

;;; Code:

(defconst qq-guild-channel-type-spec-alist
  '(("text"
     :label "文字" :icon "#" :open-mode timeline
     :history sequence :sendable t :searchable t)
    ("forum"
     :label "论坛" :icon "▤" :open-mode forum
     :history opaque-cursor :publishable t)
    ("live"
     :label "直播" :icon "◉" :open-mode inspect)
    ("application"
     :label "应用" :icon "◇" :open-mode inspect)
    ("schedule"
     :label "日程" :icon "◷" :open-mode inspect))
  "Closed QQ Guild channel kind to capability plist mapping.")

(defun qq-guild-channel-type-spec (kind)
  "Return the capability plist for closed protocol KIND."
  (cdr (assoc kind qq-guild-channel-type-spec-alist)))

(defun qq-guild-channel-type-get (kind property)
  "Return PROPERTY from closed channel KIND's capability specification."
  (plist-get (qq-guild-channel-type-spec kind) property))

(defun qq-guild-channel-type-label (kind)
  "Return the human-readable label for closed channel KIND."
  (or (qq-guild-channel-type-get kind :label)
      (error "qq: unknown Guild channel kind %S" kind)))

(defun qq-guild-channel-type-icon (kind)
  "Return the directory icon for closed channel KIND."
  (or (qq-guild-channel-type-get kind :icon)
      (error "qq: unknown Guild channel kind %S" kind)))

(defun qq-guild-channel-open-mode (kind)
  "Return the semantic open mode for closed channel KIND."
  (or (qq-guild-channel-type-get kind :open-mode)
      (error "qq: unknown Guild channel kind %S" kind)))

(defun qq-guild-channel-sendable-p (kind)
  "Return non-nil when closed channel KIND owns a message composer."
  (eq t (qq-guild-channel-type-get kind :sendable)))

(provide 'qq-guild-channel-type)

;;; qq-guild-channel-type.el ends here
