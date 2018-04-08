(in-package #:python3-cffi.test)

;; SELF is NIL.  ARGS is NIL.  (Or a null pointer, if no translation.)
(python3.cffi::defpycallback test-no-arguments python3.cffi::tuple ()
  (format t "arg: self=~A args=~A~%" python3.cffi::self python3.cffi::args)
  (list python3.cffi::self python3.cffi::args))

;; SELF tends to be NIL.  ARGS is a list of arguments.
(python3.cffi::defpycallback test-arguments python3.cffi::tuple ((arg1 (bool :borrowed)))
  (format t "arg: self=~A args=~A arg1=~A~%"
          python3.cffi::self
          python3.cffi::args
          arg1)
  (list python3.cffi::self
        python3.cffi::args
        arg1))

;; called as test_key_args(a=1, b=2, ...)
;; * SELF is NIL.  ARGS is an empty array.  DICT is a hashtable of name=value pairs
;; HOWEVER, if called as test_key_args(1, 2, a=3, b=4, ...)
;; * SELF is NIL.  ARGS is #(1 2).  DICT is a hashtable {a=3 b=4}
(python3.cffi::defpycallback test-key-args python3.cffi::tuple
    (&key (arg1 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A arg1=~A~%"
          python3.cffi::self
          python3.cffi::args
          python3.cffi::dict
          arg1)
  (list python3.cffi::self
        python3.cffi::args
        python3.cffi::dict
        arg1))

;; SELF is NIL.  ARGS is an array of the positional (non-keyword) parameters.
;; DICT is a hashtable of the keyword parameters.
(python3.cffi::defpycallback test-pos+key-args python3.cffi::tuple
    ((arg1 (bool :borrowed)) &key (arg2 (bool :borrowed)))
  (format t "key: self=~A args=~A dict=~A arg1=~A arg2=~A~%"
          python3.cffi::self
          python3.cffi::args
          python3.cffi::dict
          arg1
          arg2)
  (list python3.cffi::self
        python3.cffi::args
        python3.cffi::dict
        arg1
        arg2))

(python3.cffi::defpycallback test-no-translation :pointer ()
  (python3.cffi::null-pointer))

(defun make-callbacks-test-module ()
  (python3.cffi::build-module "callbacks_test"
   '(("no_args"   .   test-no-arguments)
     ("args"      .   test-arguments)
     ("key_args"  .   test-key-args)
     ("pos_key_args" . test-pos+key-args)
     ("no_trans"  .   test-no-translation))))

(addtest (burgled-batteries)
  callbacks-test

  (make-callbacks-test-module)

  (burgled-batteries:import "callbacks_test")

  ;; callbacks_test.no_args()
  (lift:ensure (equalp (burgled-batteries:run "callbacks_test.no_args()")
                       '(nil nil)))

  ;; callbacks_test.args(22, 'a')
  (lift:ensure (equalp (burgled-batteries:run "callbacks_test.args(22, 'a')")
                       '(nil (22 "a") 22)))

  ;; callbacks_test.key_args(arg1='foo')
  (destructuring-bind (self args dict arg1)
      (burgled-batteries:run "callbacks_test.key_args(arg1='foo')")
    (lift:ensure (equalp args nil))
    (lift:ensure (equalp (gethash "arg1" dict) "foo"))
    (lift:ensure (equalp arg1 "foo"))))
