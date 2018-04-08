(in-package #:python3-cffi.test)

(defun a-non-nul-char ()
  (generate #'(lambda () (code-char (1+ (random 255))))))
(defun a-string-without-nuls ()
  (coerce (loop :repeat (random *size*) :collect (a-non-nul-char))
          'string))
(defun a-unicode-char ()
  (generate #'(lambda () (code-char (random 16384)))))
(defun a-unicode-string ()
  (coerce (loop :repeat (random *size*) :collect (a-unicode-char))
          'string))

(defun random-list (length min max)
  (loop :repeat length :collect (+ min (random (- max min)))))
(defun an-octet-array ()
  (let ((size (random *size*)))
    (make-array size :element-type '(unsigned-byte 8)
                     :initial-contents (random-list size 0 256))))

(defun a-vector ()
  (let ((size (random *size*)))
    (make-array size :element-type 'integer
                     :initial-contents (random-list size -2048 2048))))

(defun a-hash ()
  (let ((hash (make-hash-table :test #'equal)))
    (dotimes (i (random *size*) hash)
      (setf (gethash (a-string-without-nuls) hash) i))))

(addtest (burgled-batteries)
  sequences-round-trip
  (quickcheck
    ;; See the PyString warning in ffi-interface about known-issues not covered by
    ;; quickcheck.
    (for-all ((v #'an-octet-array))
      (is equalp v (python3.cffi:byte-array.from-string-and-size v (length v))))
    (for-all ((v #'a-unicode-string))
      (is= v (python3.cffi:unicode.from-unicode v (length v))))
    (for-all ((v (a-list an-integer)))
      (let ((tuple (cffi:convert-to-foreign v 'python3.cffi::tuple)))
        (is equalp v (cffi:convert-from-foreign tuple 'python3.cffi::tuple))
        (python3.cffi:.dec-ref tuple)))
    (for-all ((v #'a-vector))
      (let ((list (cffi:convert-to-foreign v 'python3.cffi::list)))
        (is equalp v (python3.cffi:list.get-slice list 0 (length v)))))
    (for-all ((v #'a-hash))
      (let ((dict (cffi:convert-to-foreign v 'python3.cffi::dict)))
        (is equalp v (python3.cffi:dict.copy dict))))))
