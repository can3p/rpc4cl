(in-package #:rpc4cl)

(defvar *debug* nil
  "Setting this variable to t will result in debug information emitted
  on *standard-output* while calling xml-rpc-call")

(defvar *xml-rpc-interfaces* nil)

(defvar *empty-array* (gensym "empty-array")
  "Unique value used to represent an empty array")