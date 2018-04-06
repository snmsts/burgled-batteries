(in-package #:python.cffi)

(cc-flags #.(format nil "-I~A" *cpython-include-dir*))

(include "Python.h")
(include "structmember.h") ; needed for member type flags

;; Sizes
(ctype ssize-t "Py_ssize_t")
(ctype size-t  "size_t")

;; Evaluation Context Flags
(constantenum parser-context
  ((:expression "Py_eval_input"))
  ((:statement  "Py_single_input"))
  ((:file       "Py_file_input")))

;; Profiling and Tracing
(constantenum trace-what
  ((:call        "PyTrace_CALL"))
  ((:exception   "PyTrace_EXCEPTION"))
  ((:line        "PyTrace_LINE"))
  ((:return      "PyTrace_RETURN"))
  ((:c-call      "PyTrace_C_CALL"))
  ((:c-exception "PyTrace_C_EXCEPTION"))
  ((:c-return    "PyTrace_C_RETURN")))

;; Comparison Operator Flags
(constantenum comparison-operator
  ((:<  "Py_LT"))
  ((:<= "Py_LE"))
  ((:=  "Py_EQ"))
  ((:/= "Py_NE"))
  ((:>  "Py_GT"))
  ((:>= "Py_GE")))

;; Method Call Flags
(bitfield method-convention-flags
  ;; calling convention
  ((:positional-arguments "METH_VARARGS"))
  ((:keyword-arguments    "METH_KEYWORDS"))
  ((:mixed-arguments      "METH_KEYWORDS | METH_VARARGS"))
  ((:no-arguments         "METH_NOARGS"))
  ((:object-method        "METH_O"))
  ;; binding convention
  ((:class-binding  "METH_CLASS"))
  ((:static-binding "METH_STATIC"))
  ;; replace existing definition
  ((:coexist          "METH_COEXIST"))
  ((:replace-existing "METH_COEXIST")))

;; Member Type Flags
(constantenum member-type
  ((:short              "T_SHORT"))
  ((:int                "T_INT"))
  ((:long               "T_LONG"))
  ((:float              "T_FLOAT"))
  ((:double             "T_DOUBLE"))
  ((:string             "T_STRING"))
  ((:object             "T_OBJECT"))
  ((:object-ex          "T_OBJECT_EX"))
  ((:char               "T_CHAR"))
  ((:byte               "T_BYTE"))
  ((:unsigned-byte      "T_UBYTE"))
  ((:unsigned-int       "T_UINT"))
  ((:unsigned-short     "T_USHORT"))
  ((:unsigned-long      "T_ULONG"))
  ((:boolean            "T_BOOL"))
  ((:long-long          "T_LONGLONG")  :optional t)
  ((:unsigned-long-long "T_ULONGLONG") :optional t)
  ((:ssize-t            "T_PYSSIZET")))

;; API Version
(constant (+abi-version+ "PYTHON_ABI_VERSION") :type integer)
(constant (+api-version+ "PYTHON_API_VERSION") :type integer)
