(in-package #:rpc4cl)

(defun decode-method-call-response (document)
  "Decode a method call response and throw a xml-rpc-fault error if
there is a fault in the response xml otherwise return the decoded
params as multiple values."
  (unless (or (find-element document "methodResponse") (find-element document "methodCall"))
    (error 'xml-rpc-fault :fault-code 1 :fault-string "Invalid XML-RPC document"))
  (let ((fault (find-element (find-element document "methodResponse") "fault")))
    (when fault
      (let ((fault-struct (decode-struct (first-element (first-element fault)))))
        (error 'xml-rpc-fault :fault-code (getf fault-struct :fault-code) :fault-string (getf fault-struct :fault-string)))))
  (decode-params document))

(defun decode-params (document &aux params)
  "Decode all the params one by one and return as multiple values"
  (dom:do-node-list (param (elements (find-element (first-element document) "params")))
    (push (decode-value (first-element (find-element param "value"))) params))
  (values-list (reverse params)))

(defun decode-value (val)
  "Decode the value by looking at the type node name"
  (let ((type (intern (string-upcase (dom:local-name val)) :keyword)))
    (symbol-macrolet ((string-value (if (dom:first-child val) (dom:data (dom:first-child val)) "")))
      (case type
        (:int (parse-integer string-value))
        (:i4  (parse-integer string-value))
        (:boolean (= (parse-integer string-value) 1))
        (:double (parse-number:parse-real-number string-value))
        (:string (decode-string string-value))
        (:datetime.iso8601 (rfc3339:parse-string string-value))
        (:base64 (list :base64 string-value))
        (:array (decode-array val))
        (:struct (decode-struct val))
        (otherwise (decode-string string-value))))))

(defun decode-array (array-dom &aux array)
  "Decode an array to a list"
  (dom:do-node-list (array-value (elements (find-element array-dom "data")))
    (push (decode-value (first-element array-value)) array))
  (reverse array))

(defun decode-struct (val &aux struct)
  "Decode a struct to a plist"
  (dom:do-node-list (member (elements val))
    (push (de-camel-craze-keyword (dom:data (dom:first-child (find-element member "name")))) struct)
    (push (decode-value (first-element (find-element member "value"))) struct))
  (reverse struct))

(defun decode-string (string)
  "Restore any escaped string values"
  (cl-ppcre:regex-replace-all "&amp;" (cl-ppcre:regex-replace-all "&lt;" string "<") "&;"))
