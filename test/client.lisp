(in-package #:rpc4cl-test)

(defun call (method &rest method-parameters)
 "Shorthand to call our test service started
with (rpc4cl-test:star-test-server)"
 (apply #'rpc4cl:rpc-call "http://localhost:8008/validation"
  nil nil method method-parameters))

(defun run-test-client ()
 "Run lisp-unit tests"
 (run-all-tests :rpc4cl-test))

(define-test array-of-structs-test
 (assert-equal
  (call "validator1.arrayOfStructsTest"
   '((:larry 10 :curly 1 :moe 5)
     (:larry 10 :curly 8 :moe 5)
     (:larry 10 :curly 5 :moe 5))) 14))
    
(define-test count-the-entities
 (let ((entities (call "validator1.countTheEntities" "<<<&&>'''\"'\"'>>")))
  (assert-equal (getf entities :ct-left-angle-brackets) 3)
  (assert-equal (getf entities :ct-right-angle-brackets) 3)
  (assert-equal (getf entities :ct-ampersands) 2)
  (assert-equal (getf entities :ct-apostrophes) 5)
  (assert-equal (getf entities :ct-quotes) 2)))

(define-test easy-struct-test
 (assert-equal (call "validator1.easyStructTest" '(:moe 1 :curly 2 :larry 3)) 6))

(define-test echo-struct-test
 (assert-equal (call "validator1.echoStructTest" '(:a 1 :b 2)) '(:a 1 :b 2)))

(define-test many-types-test
 (let* ((date (rfc3339:make-timestamp))
        (response (call "validator1.manyTypesTest" 4 t "Hello Värld" 8.32 date "ÅÄÖ")))
  (assert-equal (nth 0 response) 4)
  (assert-equal (nth 1 response) t)
  (assert-equal (nth 2 response) "Hello Värld")
  (assert-equal (nth 3 response) 8.32)
  (assert-equal (rfc3339:local-time-of (nth 4 response)) (rfc3339:local-time-of date))
  (assert-equal (nth 5 response) "ÅÄÖ")))

(define-test moderate-size-array-check
 (let* ((words (loop
                :with words = (list "Hello ")
                :for n :from 1 :to 150
                :do (setf words (append words (list "irrelevant")))
                :finally (return (append words (list "World")))))
        (greeting (call "validator1.moderateSizeArrayCheck" words)))
  (assert-equal greeting "Hello World")))

(define-test nested-struct-test
 (let* ((calendar '(:2000 (:04 (:01 (:curly 1 :moe 4 :larry 8)))))
        (sum (call "validator1.nestedStructTest" calendar)))
  (assert-equal sum 13)))

(define-test simple-struct-return-test
 (let ((m1 (call "validator1.simpleStructReturnTest" 12.5))
       (m2 (call "validator1.simpleStructReturnTest" 12)))
  (assert-equal (getf m1 :times10) 125.0)
  (assert-equal (getf m1 :times100) 1250.0)
  (assert-equal (getf m1 :times1000) 12500.0)
  (assert-equal (getf m2 :times10) 120)
  (assert-equal (getf m2 :times100) 1200)
  (assert-equal (getf m2 :times1000) 12000)))

(define-test multicall-test
 (let ((multicall (make-instance 'rpc4cl:multicall :uri "http://localhost:8008/validation")))
   (rpc4cl:add-call multicall "validator1.echoStructTest" '(:a 1 :b 2))
   (rpc4cl:add-call multicall "validator1.easyStructTest" '(:moe 1 :curly 2 :larry 3))
   (rpc4cl:add-call multicall "validator1.echoStructTest" '(:c 3 :d 4))
   (assert-equal (rpc4cl:perform-calls multicall) '((:a 1 :b 2) 6 (:c 3 :d 4)))))

(define-test timeout-test
 (assert-error 'rpc4cl:xml-rpc-fault (call "validator1.timeoutTest")))

(define-test int-instead-of-struct-test
 (assert-error 'rpc4cl:xml-rpc-fault (call "validator1.easyStructTest" 1)))

(define-test empty-string-length-test
 (assert-equal 0 (call "validator1.stringLength" "")))

(define-test method-help-test
 (assert-equal "Return the length of the string argument"
  (call "system.methodHelp" "validator1.stringLength")))

(define-test system-list-methods-test
 (assert-true (> (length (call "system.listMethods")) 10)))

(define-test system-get-capabilities-test
 (let ((capabilities (call "system.getCapabilities")))
  (assert-equal (getf (getf capabilities :introspect) :spec-url)
   "http://xmlrpc-c.sourceforge.net/xmlrpc-c/introspection.html")
  (assert-equal (getf (getf capabilities :introspect) :spec-version) 1)))

(define-test system-method-signature-test
 (assert-equal "undef" (call "system.methodSignature" "system.listMethods")))

   