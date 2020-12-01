(in-package :cl-user)

;; The package of the XML-RPC implementation.

(defpackage #:rpc4cl
  (:use :cl)
  (:import-from :trivial-timeout :with-timeout)
  (:export
   
   ;; Interface for consuming an XML-RPC API.
   :*debug*
   :rpc-call
   :empty-array
   :multicall
   :add-call
   :perform-calls
   
   ;; Interface for providing an XML-RPC API.
   :with-api
   :handle-rpc-call
   :define-rpc-call
   :define-rpc-call-help
   
   ;; Exceptions that might be thrown by calling functions in :xml-rpc
   ;; if you are unlucky.
   :xml-rpc-fault
   :xml-rpc-parse-error))
