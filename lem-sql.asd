(asdf:defsystem "lem-sql"
  :author "garlic0x1"
  :license "MIT"
  :depends-on (:sqlite :lem :alexandria)
  :components ((:module "src"
                :components 
                ((:file "client")
                 (:file "interface")))))