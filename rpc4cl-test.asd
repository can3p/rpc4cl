(defsystem rpc4cl-test
 :name "XML RPC test suite"
 :version "0.3.2"
 :license "BSD"
 :author "Peter Stiernström <peter@stiernstrom.se>"
 :description "Implementation of a test suite server with the additional dependency on hunchentoot"
 :depends-on ( :rpc4cl :hunchentoot :lisp-unit )
 :serial t
 :components ((:module :test
               :serial t
               :components ((:file "package")
                            (:file "server")
                            (:file "client")))))

