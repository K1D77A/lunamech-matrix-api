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
    :initform :send)
   (requiredp 
    :accessor requiredp
    :initarg :requiredp
    :initform nil))
  (:documentation "A toplevel class used to define new webhooks."))

(defgeneric validate-slot (slot)
  (:method-combination progn :most-specific-first))

(defmethod validate-slot progn ((slot api-slot))
  (with-accessors ((requiredp requiredp)
                   (category category))
      slot
    (cond ((and (not (eq category :send))
                requiredp)
           (error "Cannot be requiredp but not have the category :send")))))

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
   (one-of
    :accessor one-of
    :initarg :one-of
    :initform nil
    :documentation "A list of potential values.")
   (specialp
    :reader specialp
    :initarg :specialp
    :initform nil)
   (query-param-p
    :accessor query-param-p
    :initarg :query-param-p
    :initform nil)))

(defmethod validate-slot progn ((slot api-send-slot))
  "Makes sure a slot is conforming."
  (with-accessors ((in-url-p in-url-p)
                   (query-param-p query-param-p)
                   (specialp specialp))
      slot 
    (cond ((and in-url-p query-param-p specialp)
           (error "cannot be in url a query param and a special at the same time"))
          ((and in-url-p query-param-p)
           (error "only one of in-url-p or query-param-p should be t"))
          ((and specialp (or in-url-p query-param-p))
           (error "neither in-url-p or query-param-p can be t when slot is specialp")))))

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
   (do-not-decode-p
     :accessor do-not-decode-p
     :initarg :do-not-decode-p
     :initform nil
     :documentation "Set to t and this will not be parsed as json.")
   (api
    :accessor api
    :initarg :api
    :initform "/_matrix/client/r0/")
   (request-fun
    :reader request-fun
    :initarg :request-fun)
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
   (required-slots 
    :accessor required-slots
    :initform nil)
   (content-type
    :reader content-type
    :initarg :content-type
    :initform "application/json; charset=utf-8"
    :documentation "Change to a symbol and that slot will be used for the content type 
instead")
   (contains-txn-p
    :accessor contains-txn-p
    :initform nil
    :documentation "When set to t then TXN is taken from connection when executed.")
   (specialp
    :reader specialp
    :initarg :specialp
    :initform nil)
   (special-slot
    :reader special-slot
    :initform nil)))


