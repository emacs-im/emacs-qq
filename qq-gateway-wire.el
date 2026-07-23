;;; qq-gateway-wire.el --- Native Gateway wire values -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; JSON decoding and ownership helpers for the native Gateway boundary.
;; Wire arrays stay distinguishable from objects, and JSON null stays
;; distinguishable from both until the RPC or event boundary domainizes it.

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst qq-gateway-wire-null (make-symbol "qq-gateway-wire-null")
  "Private value used to represent JSON null at the Gateway boundary.")

(defun qq-gateway-wire-null-p (value)
  "Return non-nil when VALUE is the Gateway JSON null sentinel."
  (eq value qq-gateway-wire-null))

(defun qq-gateway-wire-object-p (value)
  "Return non-nil when VALUE has the shape of a decoded JSON object."
  (and (listp value)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 value)))

(defun qq-gateway-wire-exact-object-keys-p (object keys)
  "Return non-nil when decoded OBJECT has exactly symbol KEYS."
  (and (qq-gateway-wire-object-p object)
       (equal
        (sort (mapcar #'car object)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right))))
        (sort (copy-sequence keys)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right)))))))

(defun qq-gateway-wire--copy (value domain-p)
  "Recursively copy VALUE.

When DOMAIN-P is non-nil, turn wire null into nil and wire vectors into
lists.  The supported compound values are the ones used at Gateway API
boundaries: conses, vectors, and hash tables."
  (cond
   ((and domain-p (qq-gateway-wire-null-p value)) nil)
   ((stringp value) (copy-sequence value))
   ((consp value)
    (cons (qq-gateway-wire--copy (car value) domain-p)
          (qq-gateway-wire--copy (cdr value) domain-p)))
   ((vectorp value)
    (let ((items (mapcar (lambda (item)
                           (qq-gateway-wire--copy item domain-p))
                         (append value nil))))
      (if domain-p items (vconcat items))))
   ((hash-table-p value)
    (let ((copy (make-hash-table
                 :test (hash-table-test value)
                 :size (hash-table-size value)
                 :rehash-size (hash-table-rehash-size value)
                 :rehash-threshold (hash-table-rehash-threshold value)
                 :weakness (hash-table-weakness value))))
      (maphash (lambda (key item)
                 (puthash (qq-gateway-wire--copy key domain-p)
                          (qq-gateway-wire--copy item domain-p)
                          copy))
               value)
      copy))
   (t value)))

(defun qq-gateway-value-copy (value)
  "Return a recursive ownership copy of Gateway domain VALUE."
  (qq-gateway-wire--copy value nil))

(defun qq-gateway-wire-domain-copy (value)
  "Copy accepted wire VALUE into its public Gateway domain form.

JSON null becomes nil and JSON array vectors become lists recursively."
  (qq-gateway-wire--copy value t))

(defun qq-gateway-wire-array (value context &optional allow-domain-list)
  "Validate wire array VALUE for CONTEXT and return a domain list.

Vectors are the only raw representation produced by the Gateway decoder.
When ALLOW-DOMAIN-LIST is non-nil, proper lists are also accepted for an
explicitly internal, already-normalized registry boundary.  This opt-in is
necessary because an empty decoded JSON object is also represented by nil and
must never pass a raw array boundary."
  (cond
   ((vectorp value)
    (mapcar #'qq-gateway-value-copy (append value nil)))
   ((and allow-domain-list (proper-list-p value))
    (qq-gateway-value-copy value))
   (t (error "qq: %s must be an array" context))))

(defun qq-gateway-wire-decode (text)
  "Decode Gateway JSON TEXT without collapsing wire value kinds."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'array
                     :false-object :false
                     :null-object qq-gateway-wire-null))

(provide 'qq-gateway-wire)
;;; qq-gateway-wire.el ends here
