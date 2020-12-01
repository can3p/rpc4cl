(in-package #:rpc4cl)

(define-rpc-call :system (system.get-capabilities)
 "Return suggested response to indicate support for XML-RPC
introspection."
 (list :introspect
  (list :spec-url "http://xmlrpc-c.sourceforge.net/xmlrpc-c/introspection.html"
   :spec-version 1)))

(define-rpc-call-help :system (system.get-capabilities)
  "Return system capabilities")

(define-rpc-call :system (system.method-signature :string method)
 "Not implemented but might be supported with some extension to the
define-rpc-call function."
  (assert-method method)
 "undef")

(define-rpc-call-help :system (system.method-signature)
  "Not implemented")

(define-rpc-call :system (system.list-methods)
 "system.listMethods list all methods in *xml-rpc-api-package* as
 well as those in the :system package"
  (mapcar #'xml-rpc-method-name
    (append
      (getf *xml-rpc-interfaces* *current-api*)  
      (getf *xml-rpc-interfaces* :system))))

(define-rpc-call-help :system (system.list-methods)
  "When called with no arguments returns all callable methods")

(define-rpc-call :system (system.method-help :string method-name)
 "If there is a function named method-help-for.method in
 *xml-rpc-api-package* it will be called and its output returned"
 (assert-method method-name)
  (let ((method (find-xml-rpc-method method-name)))
    (when method
      (xml-rpc-method-documentation method))))

(define-rpc-call-help :system (system.method-help)
  "When called with a string argument corresponding to a method, this
  call will returns its documentation if any.")

(define-rpc-call :system (system.multicall :array calls)
  "Perform multiple calls returning their responses in the same order
as they were called."
  (let (responses)
    (loop
      :for struct :in calls
      :for call-name = (getf struct :method-name)
      :for call = (find-xml-rpc-method  call-name)
      :for params = (getf struct :params) :do
      (assert-method call-name)
      (push (multiple-value-list (apply (xml-rpc-method-function call) params)) responses))
    (reverse responses)))

(define-rpc-call-help :system (system.multicall)
  "Perform multiple calls. The call return values are returned in call
  order.")