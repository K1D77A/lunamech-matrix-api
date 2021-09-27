(asdf:defsystem #:lunamech-matrix-api
  :description "An implementation of the Matrix API taken from LunaMech see https://lunamech.com"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador
               #:drakma
               #:jonathan
               #:str
               #:do-urlencode)
  :pathname "api"
  :components ((:file "package")
               (:file "api-helpers")
               (:file "user-api")
               (:file "spaces")
               (:file "sync")
               (:file "admin-api")
               (:file "events")))
