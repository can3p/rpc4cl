(in-package #:rpc4cl)

(defun encode-value (thing)
  "Convert lisp values to the closest XML-RPC supported type. For
complex data types we call out to their respective encoding routines."
  (cond
    ((subtypep (type-of thing) 'rfc3339:timestamp) (encode-date-time thing))
    ((subtypep (type-of thing) 'float) (format t "<double>~f</double>" thing))
    ((subtypep (type-of thing) 'string) (string (format t "<string>~a</string>" (encode-string thing))))
    ((subtypep (type-of thing) 'boolean) (format t "<boolean>~:[0~;1~]</boolean>" thing))
    ((subtypep (type-of thing) 'integer) (format t "<int>~d</int>" thing))
    ((subtypep (type-of thing) 'vector) (encode-array (coerce thing 'list)))
    ((eq thing (empty-array)) (encode-array nil))
    ((subtypep (type-of thing) 'list)
     (cond ((base64-p thing) (format t "<base64>~a</base64>" (second thing)))
           ((plist-p thing) (encode-struct thing))
           (t (encode-array thing))))))

(defun encode-struct (plist)
  "Encode a struct (being a plist) and convert the plist keys to their
camelcrazed representation."
  (princ "<struct>")
  (dolist (element (plist-keys plist))
    (princ "<member><name>")
    (princ (camel-craze-symbol element))
    (princ "</name><value>")
    (encode-value (getf plist element))
    (princ "</value></member>"))
  (princ "</struct>"))

(defun encode-array (list)
  "Encode an XML-RPC array from a list if you want to encode an actual
array/vector you should coerce it to list first."
  (princ "<array><data>")
  (dolist (element list)
    (princ "<value>")
    (encode-value element)
    (princ "</value>"))
  (princ "</data></array>"))

(defun encode-date-time (timestamp)
  "I haven't read up on iso8601 and what it entails but this should at
least be one valid format for encoding it."
  (format t "<dateTime.iso8601>~a</dateTime.iso8601>" (rfc3339:xml-rpc-timestamp timestamp)))

(defun encode-string (string)
  "Escape all & and < characters in a string with their respective
HTML entities."
  (cl-ppcre:regex-replace-all "&" (cl-ppcre:regex-replace-all "<" string "&lt;") "&amp;"))

(defun encode-params (&rest params)
  "Encode all arguments as a XML-RPC <params> tag"
  (princ "<params>")
  (loop :for param :in params :do
     (princ "<param><value>")
     (encode-value param)
     (princ "</value></param>"))
  (princ "</params>"))

(defun encode-method-call (method-name &rest params)
  "Create a complete XML document representing a method call to
method-name with any number of params"
  (with-output-to-string (*standard-output*)
    (princ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    (princ #\Newline)
    (princ "<methodCall><methodName>")
    (princ method-name)
    (princ "</methodName>")
    (apply #'encode-params params)
    (princ "</methodCall>")))

(defun encode-method-response (&rest params)
  "Encode the method response suitable for decoding by an XML-RPC
client. In case an xml-rpc-fault is raised we encode it and return
it."
  (princ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (princ #\Newline)
  (princ "<methodResponse>")
  (handler-case
      (apply #'encode-params params)
    (xml-rpc-fault (e)
      (encode-method-fault e)))
  (princ "</methodResponse>"))

(defun encode-method-fault (e)
  "A XML-RPC fault is a struct with a faultCode and a faultString
member inside a <fault> tag."
  (princ "<fault><value>")
  (encode-struct
   (list :fault-code (fault-code-of e)
         :fault-string (fault-string-of e)))
  (princ "</value></fault>"))
