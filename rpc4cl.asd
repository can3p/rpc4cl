(defsystem rpc4cl
 :name "XML RPC 4 CL"
 :version "0.3.2"
 :license "BSD"
 :author "Peter Stiernström <peter@stiernstrom.se>"
 :description "A Common Lisp implementation of XML-RPC using best of breed libraries"
 :depends-on ( :drakma :cxml :cl-ppcre :parse-number :rfc3339-timestamp :babel :trivial-timeout )
 :serial t
 :components ((:module :src
               :serial t
               :components ((:file "package")
                            (:file "condition")
                            (:file "variable")
                            (:file "utils")
                            (:file "xml-rpc")
                            (:file "conversion")
                            (:file "encode")
                            (:file "decode")
                            (:file "interface")
                            (:file "system")))))
