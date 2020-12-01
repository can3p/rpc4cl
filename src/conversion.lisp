(in-package #:rpc4cl)

(defun de-camel-craze (string)
  "Convert camelCase string to what we would expect for lisp symbol
name suitable for find-symbol. camelCase becomes CAMEL-CASE. This is
the reverse of camel-crazing something"
  (string-upcase
    (with-output-to-string (*standard-output*)
      (loop
         :for char :across string
         :for index :from 0 :do
         (cond
           ((and (> index 0) (upper-case-p char))
            (princ #\-)
            (princ char))
           (t
            (princ char)))))))

(defun camel-craze (string)
  "Convet a string containing dashes to its camelCased representation.
CAMEL-CASE becomes camelCase."
  (with-output-to-string (*standard-output*)
    (let (upcase-next)
      (loop
         :for char :across (string-downcase string)
         :for index :from 0 :do
         (when (= index 0)
           (setf upcase-next nil))
         (cond
           ((and (> index 0) (eql char #\-))
            (setf upcase-next t))
           (t
            (if upcase-next
                (progn
                  (princ (char-upcase char))
                  (setf upcase-next nil))
                (princ char))))))))

(defun camel-craze-symbol (symbol)
  "Do camel-crazing on a symbol returning a string"
  (camel-craze (symbol-name symbol)))

(defun de-camel-craze-keyword (string)
  "Decraze a string returning a keyword symbol"
  (intern (de-camel-craze string) :keyword))
