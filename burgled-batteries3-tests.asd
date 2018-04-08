;;; burgled-batteries3-tests.asdf --- system definition for the burgled-batteries3
;;; test suite

(defsystem "burgled-batteries3-tests"
  :name "burgled-batteries3-tests"
  :description "burgled-batteries3 tests"
  :author "pinterface <pix@kepibu.org>"
  :license "MIT"
  :serial t
  :components
  ((:module "test"
    :pathname "t/"
    :components
    ((:file "packages")
     (:file "tests")
     (:file "sanity")
     (:file "numeric")
     (:file "refcnts")
     (:file "sequences")
     (:file "callbacks")
     (:file "modules"))
    :serial t))
  :depends-on (#:burgled-batteries3 #:lift #:cl-quickcheck)
  :perform (test-op (o c) #+asdf3 (uiop:symbol-call '#:python3-cffi.test '#:run-tests)))
