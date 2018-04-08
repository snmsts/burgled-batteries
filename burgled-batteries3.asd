;;; burgled-batteries3.asd --- ASDF system definition for burgled-batteries3

#-burgled-batteries.guess-not-grovel
(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defsystem "burgled-batteries3"
    :defsystem-depends-on (#-burgled-batteries.guess-not-grovel "cffi-grovel")
    :depends-on (#:cffi #:alexandria #:parse-declarations-1.0 #:trivial-garbage
                        #-burgled-batteries.guess-not-grovel #:cl-fad)
    :name "burgled-batteries3"
    :author "pinterface <pix@kepibu.org>"
    :license "MIT"
    :description "Lisp-Python interface"
    :long-description "
This system provides support for embedding Python into Common Lisp via CFFI.

It uses cffi-grovel to determine sizes and values of some assorted Python types
and constants.  However, if you'd rather avoid this, or grovelling is not
possible for you, a best-guess effort can also be made.  You can note your
preference for guessing by evaluating the following form before telling ASDF to
load this system:
  (push :burgled-batteries3.guess-not-grovel *features*)

If you /would/ like to use the groveller, B-B will attempt to determine the
location of Python's C header files, and will prompt you to specify the
appropriate directory if one cannot be found.  To grovel against a specific
copy of Python's header files, you may need to edit
  (defparameter *cpython-include-dir* ...)
in #p\"grovel-include-dir.lisp\".
"
    :serial t
    :components
    ((:file "packages")
     #-burgled-batteries3.guess-not-grovel (:file "grovel-include-dir")
     #-burgled-batteries3.guess-not-grovel (cffi-grovel:grovel-file "grovel")
     #+burgled-batteries3.guess-not-grovel (:file "grovel-guess")
     (:file "cffi-output-args")
     (:file "ffi-definers")
     (:file "ffi-interface")
     (:file "ffi-conditions")
     (:file "ffi-callbacks")
     (:file "module")
     (:file "api"))
    :in-order-to ((test-op (test-op #:burgled-batteries3-tests))))
