(in-package #:rpc4cl)

(defun first-element (node)
  (when (or (dom:element-p node) (dom:document-p node))
    (aref (elements node) 0)))

(defun elements (node)
  (when (or (dom:element-p node) (dom:document-p node))
    (remove-if-not #'dom:element-p (dom:child-nodes node))))

(defun find-element (node name)
  (when (stringp name)
    (find-if #'(lambda (e) (equal (dom:local-name e) name)) (elements node))))

(defun assert-method (methodName)
 "Make sure that a method exists by looking it up its name"
  (unless (find-xml-rpc-method methodName)
    (error 'rpc4cl:xml-rpc-fault
      :fault-code 4
      :fault-string (format nil "No such method ~a" methodName))))

(defun find-xml-rpc-method (name)
  "Find a method in *current-api* or :system"
  (declare (special *current-api*))
  (find name
    (append
      (getf *xml-rpc-interfaces* *current-api*)
      (getf  *xml-rpc-interfaces* :system))
    :key #'xml-rpc-method-name
    :test #'equalp))

(defun plist-keys (plist)
  "Return the keys of a plist"
  (loop :for i :from 0 :for x :in plist :when (evenp i) :collect x))

(defun plist-values (plist)
  "Return the values of a plist"
  (loop :for i :from 0 :for x :in plist :when (oddp i) :collect x))

(defun plist-p (possible-plist)
  "Try to determine if possible-plist is a valid plist by making sure
the list contains even pairs and that all keys are keywords."
  (and
    (listp possible-plist)
    (= (mod (length possible-plist) 2) 0)
    (every #'keywordp (plist-keys possible-plist))))

(declaim (inline utf8-length))

(defun utf8-length (string)
  "Determine content-length for an UTF-8 string using babel for
portability."
  (values (babel:string-size-in-octets string)))