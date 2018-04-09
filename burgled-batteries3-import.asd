(defsystem "burgled-batteries3-import"
  :depends-on (#:burgled-batteries3 #:jonathan)
  :name "burgled-batteries3-import"
  :license "MIT"
  :serial t
  :components
  ((:module "import"
    :components
    ((:file "import"))
    :serial t)))
