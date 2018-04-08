(in-package #:python3-cffi.test)

(addtest (burgled-batteries)
  refcnt-same-after-translation
  (python3.cffi:with-refcnt-barrier
    (loop :for (object code) :in `((,python3.cffi:+None+  "None")
                                   (,python3.cffi:+True+  "True")
                                   (,python3.cffi:+False+ "False")
                                   ;; CPython implements small numbers as shared references.
                                   (,(burgled-batteries3:run* "1") "1"))
          :do (symbol-macrolet ((current-refcnt (python3.cffi::%object.refcnt object)))
                (let ((orig-refcnt current-refcnt))
                  (flet ((ensure-unchanged-refcnt (python-code)
                           (burgled-batteries3:run python-code)
                           (assert (= orig-refcnt current-refcnt) ()
                                   "Reference count for ~S was ~A by ~S"
                                   code
                                   (if (> orig-refcnt current-refcnt) "decreased" "increased")
                                   python-code)))
                    (ensure-unchanged-refcnt code)
                    (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
                    (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
                    (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code))))))))

;; unknown translations
(addtest (burgled-batteries)
  refcnt-same-outside-refcnt-barrier
  (python3.cffi:with-refcnt-barrier
    (burgled-batteries3:run "import datetime")
    (burgled-batteries3:run "tmp = datetime.date.today()")
    (let* ((ptr (cpython3::dict.get-item* burgled-batteries3::main-module-dict* "tmp"))
           (orig-refcnt (cpython3::%object.refcnt ptr)))
      (loop :for (object code) :in `((,ptr "tmp"))
            :do (symbol-macrolet ((current-refcnt (python3.cffi::%object.refcnt object)))
                  (let ((orig-refcnt current-refcnt))
                    (flet ((ensure-unchanged-refcnt (python-code)
                             (cpython3:with-refcnt-barrier
                               (burgled-batteries3:run python-code))
                             (assert (= orig-refcnt current-refcnt) ()
                                     "Reference count for ~S was ~A by ~S"
                                     code
                                     (if (> orig-refcnt current-refcnt) "decreased" "increased")
                                     python-code)))
                      (ensure-unchanged-refcnt code)
                      (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
                      (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
                      (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code))))))
      (assert (= orig-refcnt (cpython3::%object.refcnt ptr))
              ()
              "Reference count was changed ~D overall."
              (- orig-refcnt (cpython3::%object.refcnt ptr))))))

;; Inspired by (read: almost entirely copied from) #'VOODOO in trivial-garbage's
;; tests.
(defun voodoo (expr)
  (funcall (compile nil `(lambda () (eval (read-from-string (format nil "~S" ',expr))))))
  (values))

(addtest (burgled-batteries)
  refcnt-same-after-finalizer-runs
  (cpython3:with-unknown-translation-policy (:finalize)
    (burgled-batteries3:import "time")
    (burgled-batteries3:run "v = time.gmtime()")
    (let* ((code "v")
           (wrapped (burgled-batteries3:run code))
           (object (python3.cffi::wrapped-value wrapped)))
      (unwind-protect
           (symbol-macrolet ((current-refcnt (python3.cffi::%object.refcnt object)))
             (let ((orig-refcnt current-refcnt))
               (flet ((ensure-unchanged-refcnt (python-code)
                        (tg:gc :full t)
                        (voodoo `(burgled-batteries3:run ,python-code))
                        (voodoo `(tg:gc :full t))
                        (assert (= orig-refcnt current-refcnt) ()
                                "Reference count (~A, ~A) for ~S was ~A by ~A from ~S"
                                orig-refcnt current-refcnt
                                code
                                (if (> orig-refcnt current-refcnt) "decreased" "increased")
                                (abs (- orig-refcnt current-refcnt))
                                python-code)))
                 (ensure-unchanged-refcnt code)
                 (ensure-unchanged-refcnt (format nil "[~A, ~A, ~A]" code code code))
                 (ensure-unchanged-refcnt (format nil "(~A, ~A, ~A)" code code code))
                 (ensure-unchanged-refcnt (format nil "dict(a=~A, b=~A, c=~A)" code code code)))))
        (burgled-batteries3:run "v = None")
        (tg:gc :full t))
      ;; Mostly this is just here to ensure wrapped doesn't get GCed during the tests
      (cpython3::%object.refcnt (cpython3::wrapped-value wrapped)))))

;; unknown translations with stolen references
(addtest (burgled-batteries)
  refcnt-same-for-stolen-references
  (cpython3:with-refcnt-barrier
    (burgled-batteries3:run "import datetime")
    (burgled-batteries3:run "tmp = datetime.date.today()")
    (let* ((ptr (cpython3::dict.get-item* burgled-batteries3::main-module-dict* "tmp"))
           (orig-refcnt (cpython3::%object.refcnt ptr)))
      (loop :for (object code) :in `((,ptr "tmp"))
            :do (symbol-macrolet ((current-refcnt (python3.cffi::%object.refcnt object)))
                  (let* ((orig-refcnt current-refcnt)
                         (tuple* (burgled-batteries3:run* "(1, 2, 3, 4)")))
                    (flet ((ensure-unchanged-refcnt ()
                             (assert (= orig-refcnt current-refcnt) ()
                                     "Reference count for ~S was ~A"
                                     code
                                     (if (> orig-refcnt current-refcnt) "decreased" "increased"))))
                      (ensure-unchanged-refcnt)
                      (cpython3:with-unknown-translation-policy (:barrier)
                        (cpython3:tuple.set-item tuple* 0 (burgled-batteries3:run "tmp")))
                      (voodoo
                       `(cpython3:with-unknown-translation-policy (:finalize)
                          (cpython3:tuple.set-item ,tuple* 1 (burgled-batteries3:run "tmp"))))
                      (voodoo `(tg:gc :full t))
                      (cpython3:.dec-ref tuple*)
                      (ensure-unchanged-refcnt)))))
      (assert (= orig-refcnt (cpython3::%object.refcnt ptr))
              ()
              "Reference count was changed ~D overall."
              (- orig-refcnt (cpython3::%object.refcnt ptr))))))
