(asdf:defsystem #:lunamech-matrix-api
  :description "An implementation of the Matrix API taken from LunaMech see https://lunamech.com"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :depends-on (#:dexador
               #:drakma
               #:jonathan
               #:str
               #:plump 
               #:cl-json
               #:reader
               #:closer-mop
               #:do-urlencode)
  :pathname "api"
  :components ((:file "package")
               (:file "classes")
               (:file "conditions")
               (:file "api-helpers")
               (:file "integration")
               (:file "user-api")
               (:file "spaces")
               (:file "sync")
               (:file "admin-api")
               (:file "events")
               (:module "v2"
                :description "Version 2 of the API library."
                :serial t
                :components ((:file "api-package")
                             (:file "classes")
                             (:module "protocol"
                              :description "The MOP protocol"
                              :serial t
                              :components ((:file "classes")
                                           (:file "conditions")
                                           (:file "call-wrapper")
                                           (:file "api-protocol")))
                             (:file "conditions")
                             (:module "2"
                              :serial t
                              :components ((:file "api")))
                             (:module "4"
                              :serial t
                              :components ((:file "api")))
                             (:module "5"
                              :serial t
                              :components ((:file "api")))
                             (:module "6"
                              :serial t
                              :components ((:file "api")))
                             (:module "8"
                              :serial t
                              :components ((:file "api")))
                             (:module "9"
                              :serial t
                              :components ((:file "api")))
                             (:module "10"
                              :serial t
                              :components ((:file "api")))
                             (:module "11"
                              :serial t
                              :components ((:file "api")))
                             (:module "13"
                              :serial t
                              :components ((:file "api")))
                             (:module "admin"
                              :serial t
                              :components ((:file "api")))
                             (:module "spaces"
                              :serial t
                              :components ((:file "api")))
                             (:file "object-helpers")
                             (:file "filters")
                             (:file "integration")
                             (:file "sync")
                             (:file "user-api")))))
