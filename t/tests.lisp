(in-package #:python3-cffi.test)

(lift:deftestsuite burgled-batteries ()
  ()
  (:setup (burgled-batteries3:startup-python))
  (:teardown (burgled-batteries3:shutdown-python)))

(defun run-tests ()
  (lift:run-tests :suite 'burgled-batteries))

(defmacro assert (predicate _ report &rest arguments)
  "CL:ASSERT compatibility Wrapper around LIFT:ENSURE."
  (declare (ignore _))
  `(lift:ensure ,predicate :report ,report :arguments (,@arguments)))
