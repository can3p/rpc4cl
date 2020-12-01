(in-package #:rpc4cl)

(defstruct xml-rpc-method name function documentation)

(defun xml-rpc-exchange (uri xml &optional username password &aux (content-length (utf8-length xml)))
  "Use drakma to perform an HTTP POST request with the supplied xml in
the body and optionally with basic auth if username and password are
non-nil."
  (drakma:http-request
   uri
   :method :post
   :basic-authorization (when (and username password) (list username password))
   :content-type "text/xml; charset=UTF-8"
   :content-length content-length
   :external-format-out :utf-8
   :keep-alive nil
   :close t
   :content xml))

(defun base64-p (list)
  "We expect base64 encoded content to be denoted as a two element
lisp with the first element being the keyword :base64 and the second
being the base64 encoded string contents."
  (and (= (length list) 2)
       (stringp (second list))
       (eq (first list) :base64)))

(defun xml-rpc-method-equalp (a b)
  "Method are equal if their functions match"
  (equalp (xml-rpc-method-name a) (xml-rpc-method-name b)))