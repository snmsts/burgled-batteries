(in-package #:python3.cffi)

(defun read-path ()
  (format *query-io* "Enter directory containing Python's C header files: ")
  (cl:list (read-line *query-io*)))

(defun query-error (format-string &rest args)
  (restart-case (apply #'error format-string args)
    (use-value (v)
      :report "Specify directory containing CPython's header files"
      :interactive read-path
      v)))

(defun query-user-for-include-dir ()
  (loop :for path := (query-error "Unable to determine Python include directory.")
               :then (query-error "Path ~s does not appear to exist." path)
        :when (cl-fad:directory-exists-p path) :return it))

(defvar *miniconda3* t)
(defparameter *cpython-lib*
  (let ((env-value (uiop:getenv "BB_PYTHON3_DYLIB")))
    (when env-value
      (or (and (uiop:file-exists-p env-value) (cl:list env-value))
          (error "DLL for PYTHON3 not correctly pointed to by BB_PYTHON3_DYLIB: ~A" env-value)))))

(defun detect-python ()
  (setf *detected-library* nil)
  (or
   (let ((env-value (uiop:getenv "BB_PYTHON3_INCLUDE_DIR")))
     (when env-value
       (or (uiop:directory-exists-p env-value)
           (error "BB_PYTHON3_INCLUDE_DIR is set, but does not point to an actual directory: ~a" env-value))))
   (when *miniconda3*
     (let ((path (if (pathnamep *miniconda3*)
                     *miniconda3*
                     (or
                      (probe-file (merge-pathnames "miniconda3/" (user-homedir-pathname)))
                      (probe-file "/usr/local/miniconda3/")))))
       (when path
         (loop :for minor :from 7 :downto 4
               :for it := (cl-fad:directory-exists-p (merge-pathnames (format nil "include/python3.~dm/" minor) path))
               :when it
                 :return (progn
                           (setf *cpython-lib* (cl:list
                                                (or (probe-file (merge-pathnames
                                                                 (format nil "lib/libpython3.~Am.so.1.0" minor) path))
                                                    (probe-file (merge-pathnames
                                                                 (format nil "lib/libpython3.~Am.dylib" minor) path)))))
                           it)))))
      (loop :for minor :from 7 :downto 4
         :when (or (cl-fad:directory-exists-p (format nil "/usr/include/python3.~d" minor))
                   (cl-fad:directory-exists-p (format nil "/usr/local/include/python3.~d" minor)))
         :return it)
      ;; This allows us to avoid querying the user during a recompile, while
      ;; still allowing for a change in Python version
      (when (boundp '*cpython-include-dir*)
        (cl-fad:directory-exists-p *cpython-include-dir*))
      (query-user-for-include-dir)))

(defparameter *cpython-include-dir* (detect-python))
