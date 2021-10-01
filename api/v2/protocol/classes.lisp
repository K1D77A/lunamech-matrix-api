(in-package #:lunamech-matrix-api/v2)

(defclass api-slot (c2mop:slot-definition)
  ((name
    :accessor name
    :initarg :name)
   (name->json
    :accessor name->json 
    :initarg :name->json
    :initform nil)
   (category 
    :accessor category
    :initarg :category
    :initform nil)
   (requiredp 
    :accessor requiredp
    :initarg :requiredp
    :initform t))
  (:documentation "A toplevel class used to define new webhooks."))

(defclass api-send-slot (api-slot)
  ((encoder
    :accessor encoder
    :initarg :encoder
    :initform #'url-e
    :documentation "if this slot is being encoded into the URL then uses this function.")
   (in-url-p
    :accessor in-url-p
    :initarg :in-url-p
    :initform nil)
   (category
    :initform :send)
   (specialp
    :reader specialp
    :initarg :specialp
    :initform nil)))

(defclass api-send-slot-special (api-send-slot)
  ((specialp
    :initform t)))

(defclass api-call (c2mop:funcallable-standard-class)
  ((connection
    :accessor connection
    :initarg :connection)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :documentation "A URL with :<slot-name> within where the slot value is encoded.")
   (api
    :accessor api
    :initarg :api
    :initform "/_matrix/client/r0/")
   (rate-limited-p
    :accessor rate-limited-p
    :initarg :rate-limited-p
    :initform t)
   (requires-auth-p
    :accessor requires-auth-p
    :initarg :requires-auth-p
    :initform t)
   (string-constructor
    :accessor string-constructor
    :initform nil)
   (request-type
    :reader request-type
    :initarg :request-type
    :initform :post)
   (required-slots 
    :accessor required-slots
    :initform nil)
   (content-type
    :reader content-type
    :initform "application/json; charset=utf-8")
   (specialp
    :reader specialp
    :initarg :specialp
    :initform nil)
   (special-slot
    :reader special-slot
    :initform nil)))
