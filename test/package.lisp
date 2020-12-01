(in-package #:cl-user)

;; Package that implements the validation suite as described on the
;; xml-rpc.com site. validator.xml-rpc.com doesn't seem to be up
;; anymore so we will have to verify manually with a client using
;; another implementation of xml-rpc.

(defpackage #:rpc4cl-test
 (:use #:cl #:lisp-unit)
 (:import-from #:rpc4cl #:define-rpc-call #:define-rpc-call-help #:with-api)
 (:import-from #:flexi-streams #:make-external-format)
 (:export
  #:start-test-server
  #:stop-test-server
  #:run-test-client))
