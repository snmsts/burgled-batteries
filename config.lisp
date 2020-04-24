(in-package #:cpython3)

;;;; Utilities for extracting configuration information from the `python3-config` command line tool
;; Inspired by and templated from GSL-CONFIG in GSLL 
(defun python3-config (arg &optional path-opt)
  "A wrapper for tool `python3-config'."
  (or
   (ignore-errors
     (let ((output (with-input-from-string
                       (s (with-output-to-string (asdf::*verbose-out*)
                            (uiop:run-program `("python3-config" ,arg) :output asdf::*verbose-out*)))
                     (read-line s)))
           (len (length path-opt)))
       (if (null path-opt)
         output
         (when (eql len (mismatch output path-opt :test #'string=))
           (uiop:ensure-directory-pathname
	    (uiop:ensure-absolute-pathname
	     (pathname
	      (subseq output len (position #\space output)))))))))
   (warn "Error attempting to run the `python3-config` command")))
