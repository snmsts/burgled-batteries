(defpackage #:burgled-batteries3-import
  (:use #:cl #:python3.cffi)
  (:export #:defpypackage))

(in-package #:burgled-batteries3-import)

;; automatic inport
(defpackage pyntern)
(defvar *serialize* `(:json ("json.dumps" jonathan:parse)
                            :debug ("str" identity)))

(defun pyntern (str)
  (let ((*package* (find-package :pyntern)))
    (cffi:translate-name-from-foreign str *package*)))

(defmacro defpyfun (name &key symbol)
  (destructuring-bind (args varargs varkw defaults knowlyargs kwonlydefaults annotations &rest r)
      (burgled-batteries3:run (format nil "inspect.getfullargspec (~A)" name))
    (let ((serialize (gensym "SERIALIZE")))
      `(defun ,(or symbol
                   (read-from-string name))
           (,@(when varkw (list '&rest (pyntern (format nil "**~A" varkw))))
            &key ,@(loop for i in (coerce args 'list)
                      for j from 0
                      collect (list (pyntern i) (nth j defaults) (pyntern (format nil "~A*" i))))
              ,@(when varargs (list (pyntern (format nil "*~A" varargs))))
              (serialize :json)
              &allow-other-keys)
         ,(burgled-batteries3:run (format nil "inspect.getdoc (~A)" name))
         (let ((,serialize (getf *serialize* serialize)))
           (funcall (second ,serialize)
                    (burgled-batteries3:run (format nil ,(format nil "~~A(~A(~~{~~A~~}))"
                                                                 name)
                                                    (first ,serialize)
                                                    (list 
                                                     ,@(loop for i in (coerce args 'list)
                                                          collect `(if ,(pyntern (format nil "~A*" i))
                                                                       (format nil ,(format nil "~A=~~A," i)
                                                                               (if (symbolp ,(pyntern i))
                                                                                   (string ,(pyntern i))
                                                                                   (format nil "~S" ,(pyntern i))))
                                                                       "")))))))))))

(defmacro defpylib (lib &key (ignore-errors t) export prefix)
  `(progn
     (burgled-batteries3:import "inspect")
     (burgled-batteries3:import ,lib)
     ,@(loop for i in (mapcar #'car (coerce (burgled-batteries3:run (format nil "inspect.getmembers(~A,inspect.isfunction)" lib)) 'list))
          collect `(,(if ignore-errors 'ignore-errors 'progn)
                     (,(if export 'export 'progn)
                       (defpyfun ,(format nil "~A.~A" lib i)
                           :symbol ,(when prefix 
                                      (read-from-string i)))))
          ;;(burgled-batteries3:run (format nil "str(inspect.getmodule (~A.~A))" lib i))
            )
     ,@(progn
         (burgled-batteries3:run (format nil "inspect.getmembers(~A,inspect.isbuiltin)" lib))
         nil)
     ,lib))

(defmacro defpypackage (name)
  (let ((package (read-from-string (format nil "#:py.~A" name))))
    (defpackage package)
    `(progn
       (defpackage ,package)
       ,(let ((*package* (find-package package)))
          (macroexpand-1 `(defpylib ,name :prefix t :export t :ignore-errors t))))))

;; (defpypackage "os")
