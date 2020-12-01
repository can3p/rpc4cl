(in-package #:rpc4cl)

;; Conditions thrown from functions in the xml-rpc package.

(define-condition xml-rpc-fault ()
  ((fault-code   :initform nil :initarg :fault-code   :reader fault-code-of)
   (fault-string :initform nil :initarg :fault-string :reader fault-string-of))
  (:documentation "Condition thrown in the event of an XML-RPC fault"))

(define-condition xml-rpc-parse-error ()
  ((fault-string :initform nil :initarg :fault-string :reader fault-string-of))
  (:documentation "Condition thrown in the event of an XML-RPC parse error"))