(in-package #:rpc4cl)

(defmacro with-api ((api-name) &body body)
 "Bind *current-api* to api-name for future reference."
 `(let ((*current-api* ,api-name))
   (declare (special *current-api*))
   ,@body))

(defmacro define-rpc-call (api-name (call-name &rest parameters) &body body)
  "Usage: (define-rpc-call :my-api (plus :int a :int b) (+ a b))

Defines a function, call-name which when called verifies its
parameters and number of parameters and throws appropriate xml-rpc
faults before executing it's body.

For 'sloppy' code the :any type may be used but then you better make
sure yourself that you've got the expected type.
"
  (assert (keywordp api-name))
  (flet ((is-a (type-keyword variable)
           (ecase type-keyword
             (:any t)
             (:int `(subtypep (type-of ,variable) 'integer))
             (:double `(subtypep (type-of ,variable) 'float))
             (:string `(stringp ,variable))
             (:boolean `(member ,variable '(t nil)))
             (:date-time `(eql (type-of ,variable) 'rfc3339:timestamp))
             (:base64 `(rpc4cl::base64-p ,variable))
             (:array `(listp ,variable))
             (:struct `(rpc4cl::plist-p ,variable)))))
    `(progn
       (defun ,call-name (,@(plist-values parameters) &rest no-such-parameters)
         (declare (special *current-api*))
         ,@(loop :for type :in (plist-keys parameters)
             :for variable :in (plist-values parameters)
             :when (not (eq type :any))
             :collect `(unless ,(is-a type variable)
                         (error 'rpc4cl:xml-rpc-fault
                           :fault-code 1
                           :fault-string (format nil "Invalid parameter-type ~a expected ~a" (type-of ,variable) ,type))))
         (when no-such-parameters
           (error 'rpc4cl:xml-rpc-fault :fault-code 2 :fault-string "Too many parameters"))
         ,@body)
       (pushnew (make-xml-rpc-method
                  :name ,(camel-craze (symbol-name call-name))
                  :function ',call-name)
         (getf *xml-rpc-interfaces* ,api-name)
         :test #'xml-rpc-method-equalp))))
    
(defmacro define-rpc-call-help (api-name (call-name) documentation)
 "Define documentation that will be returned through system.methodHelp"
  `(with-api (,api-name)
     (let ((method (find-xml-rpc-method (camel-craze (symbol-name ',call-name)))))
       (assert method)
       (setf (xml-rpc-method-documentation method) ,documentation))))

;; An example using hunchentoot might look like this:
;;
;; (define-easy-handler (rpc :uri "/rpc") ()
;;  (with-api (:my-api)
;;    (handle-xml-rpc-call (raw-post-data))))

(defun handle-rpc-call (data &key (timeout 0))
  "Handle an incoming xml-rpc call by looking up the methodName and
then calling the function with the multiple values in the decoded
<params></params>. Then encode the result to its string representation
and return it.

Optionally throw an xml-rpc-fault with code 0 if timeout was reached
before the end of the call.
"
  (with-output-to-string (*standard-output*)
    (handler-case
      (handler-case
            
        (with-timeout (timeout)
          (unless data (error 'xml-rpc-fault :fault-code 3 :fault-string "This is an XML-RPC service expecting well formed XML POST-DATA"))
              
          (let* ((document (cxml:parse data (cxml-dom:make-dom-builder)))
                  (call-name (dom:data (dom:first-child (find-element (find-element document "methodCall") "methodName"))))
                  (call (find-xml-rpc-method call-name)))

            (unless call (error 'xml-rpc-fault :fault-code 4 :fault-string (format nil "No such method ~a" call-name)))
                
            (let* ((params (multiple-value-list (decode-params document))))
              (multiple-value-call #'encode-method-response
                (apply (xml-rpc-method-function call) params)))))
        
        (trivial-timeout:timeout-error ()
          (error 'xml-rpc-fault
            :fault-code 0
            :fault-string (format nil "Operation timed out after ~d seconds" timeout)))
          
        (cxml:xml-parse-error ()
          (error 'xml-rpc-fault
            :fault-code 5
            :fault-string "Invalid XML payload"))
        
        (error (e)
          (error 'xml-rpc-fault
            :fault-code 6
            :fault-string (format nil "~a" e))))

      (xml-rpc-fault (e)
        (princ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
        (princ #\Newline)
        (princ "<methodResponse>")
        (encode-method-fault e)
        (princ "</methodResponse>")))))

(defun rpc-call (uri username password method-name &rest params)
  "Call method-name with params. The encoded call is sent to uri and
the decoded response parameters are returned as multiple values or in
the case of a fault a xml-rpc-fault condition is raised. To make this
function more ergonomic you should propbably wrap it up in your own
which specifies uri, username and password as needed for you.
"
  (assert (and (stringp uri) method-name))
  (let* ((xml (apply #'encode-method-call method-name params))
         (response (xml-rpc-exchange uri xml username password)))
    (when *debug*
      (format t "REQUEST:~%~a~%~%RESPONSE:~%~a~%" xml response))
    (handler-case 
     (decode-method-call-response (cxml:parse response (cxml-dom:make-dom-builder)))
     (cxml:xml-parse-error ()
      (error 'xml-rpc-fault :fault-code 105 :fault-string "Invalid XML response")))))

(defclass multicall ()
  ((username :initform nil :initarg :username :reader username-of)
   (password :initform nil :initarg :password :reader password-of)
   (uri      :initform (error "An URI must be supplied") :initarg :uri :reader uri-of)
   (calls    :initform nil :accessor calls-of))
  (:documentation "A stack for multiple calls using system.multicall"))

(defmethod add-call ((multicall multicall) method-name &rest params)
  "Add a call to the multicall stack"
  (push (list method-name params) (calls-of multicall)))

(defmethod perform-calls ((multicall multicall))
  "Perform all calls in the multicall stack in the order they were added"
  (assert (calls-of multicall))
 (apply #'append
  (rpc-call
   (uri-of multicall)
   (username-of multicall)
   (password-of multicall)
   "system.multicall"
   (loop :for (method-name params) :in (reverse (calls-of multicall))
    :collect (list :method-name method-name :params (if params params (rpc4cl:empty-array)))))))

(declaim (inline empty-array))

(defun empty-array ()
  "Return the special *empty-array* value"
  *empty-array*)