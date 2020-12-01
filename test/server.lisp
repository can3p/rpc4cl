(in-package #:rpc4cl-test)

;; A minimal server running on port 8008 (where client.rb and client.lisp expects it).
(let ((server (make-instance 'hunchentoot:easy-acceptor :port 8008 :access-log-destination nil :message-log-destination nil)))

 (hunchentoot:define-easy-handler (validation-test-suite :uri "/validation")
  ()
  (setf (hunchentoot:content-type*) "text/xml; charset=UTF-8")
  (with-api (:xml-rpc-tests)
   (rpc4cl:handle-rpc-call (hunchentoot:raw-post-data) :timeout 1)))
 
 (defun start-test-server ()
  (setf hunchentoot:*hunchentoot-default-external-format* (make-external-format :utf8 :eol-style :lf))
  (hunchentoot:start server))
 
 (defun stop-test-server ()
  (hunchentoot:stop server)))

;; This handler takes a single parameter, an array of structs, each of
;; which contains at least three elements named moe, larry and curly,
;; all <i4>s. Your handler must add all the struct elements named
;; curly and return the result.

(define-rpc-call :xml-rpc-tests (validator1.array-of-structs-test :array array)
 (apply #'+ (mapcar (lambda (struct) (getf struct :curly)) array)))

;; This handler takes a single parameter, a string, that contains any
;; number of predefined entities, namely <, >, &, ' and ".

;; Your handler must return a struct that contains five fields, all
;; numbers: ctLeftAngleBrackets, ctRightAngleBrackets, ctAmpersands,
;; ctApostrophes, ctQuotes.
;;
;; To validate, the numbers must be correct.

(define-rpc-call :xml-rpc-tests (validator1.count-the-entities :string string)
 (let ((ct-left-angle-brackets  (count #\< string))
       (ct-right-angle-brackets (count #\> string))
       (ct-ampersands           (count #\& string))
       (ct-apostrophes          (count #\' string))
       (ct-quotes               (count #\" string)))
  (list :ct-left-angle-brackets ct-left-angle-brackets
   :ct-right-angle-brackets ct-right-angle-brackets
   :ct-ampersands ct-ampersands
   :ct-apostrophes ct-apostrophes
   :ct-quotes ct-quotes)))

;; This handler takes a single parameter, a struct, containing at
;; least three elements named moe, larry and curly, all <i4>s. Your
;; handler must add the three numbers and return the result.

(define-rpc-call :xml-rpc-tests (validator1.easy-struct-test :struct struct)
 (+ (getf struct :moe)
  (getf struct :larry)
  (getf struct :curly)))

;; This handler takes a single parameter, a struct. Your handler must
;; return the struct.

(define-rpc-call :xml-rpc-tests (validator1.echo-struct-test :struct struct)
 struct)

;; This handler takes six parameters, and returns an array containing
;; all the parameters.
;;
;; Made the base64 encoded parameter a string because I didn't have
;; time figuring out how to make the ruby xml-rpc actually return
;; something within a base64 tag.

(define-rpc-call :xml-rpc-tests (validator1.many-types-test :any number :boolean boolean :string string :double double :date-time date-time :string base64)
 (list number boolean string double date-time base64))

;; This handler takes a single parameter, which is an array containing
;; between 100 and 200 elements. Each of the items is a string, your
;; handler must return a string containing the concatenated text of
;; the first and last elements.

(define-rpc-call :xml-rpc-tests (validator1.moderate-size-array-check :array array)
 (concatenate 'string (first array) (first (reverse array))))

;; This handler takes a single parameter, a struct, that models a
;; daily calendar. At the top level, there is one struct for each
;; year. Each year is broken down into months, and months into
;; days. Most of the days are empty in the struct you receive, but the
;; entry for April 1, 2000 contains a least three elements named moe,
;; larry and curly, all <i4>s. Your handler must add the three numbers
;; and return the result.

;; Ken MacLeod: "This description isn't clear, I expected
;; '2000.April.1' when in fact it's '2000.04.01'. Adding a note saying
;; that month and day are two-digits with leading 0s, and January is
;; 01 would help." Done.

(define-rpc-call :xml-rpc-tests (validator1.nested-struct-test :struct struct)
 (let ((april-1st (getf (getf (getf struct :2000) :04) :01)))
  (+ (getf april-1st :moe)
   (getf april-1st :larry)
   (getf april-1st :curly))))

;; This handler takes one parameter, and returns a struct containing
;; three elements, times10, times100 and times1000, the result of
;; multiplying the number by 10, 100 and 1000.

(define-rpc-call :xml-rpc-tests (validator1.simple-struct-return-test :any number)
 (list :times10   (* number 10)
  :times100  (* number 100)
  :times1000 (* number 1000)))

;; Sleep two seconds making sure that call timeout of one second is
;; reached thus throwing an XML-RPC-FAULT with code 0,

(define-rpc-call :xml-rpc-tests (validator1.timeout-test)
 (sleep 2))

;; Return length of string

(define-rpc-call :xml-rpc-tests (validator1.string-length :string string)
 (when (stringp string)
  (length string)))

(define-rpc-call-help :xml-rpc-tests (validator1.string-length)
  "Return the length of the string argument")
