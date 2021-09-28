(in-package #:lunamech-matrix-api)
#||
This file contains the code for a metaobject protocol used for 
specifying the matrix API calls.
Right now it must simply replace the (auth-req ..) functionality
as the interface cannot be changed, although all newly implemented API calls can 
follow a different scheme.
||#


;;;;this file contains a MOP for webhooks

(defclass api-slot (c2mop:slot-definition)
  ((name
    :accessor name
    :initarg :name)
   (name->json
    :accessor name->json 
    :initarg :name->json)
   (category 
    :accessor category
    :initarg :category
    :initform :result))
  (:documentation "A toplevel class used to define new webhooks."))

(defclass api-send-slot (api-slot)
  ((encoder
    :accessor encoder
    :initarg :encoder
    :initform #'url-e
    :documentation "if this slot is being encoded into the URL then uses this function.")
   (category
    :initform :send)))

(defclass api-receive-slot (api-slot)
  ((category
    :initform :receive)))

(defclass api-call (standard-class)
  ((connection
    :accessor connection
    :initarg :connection)
   (endpoint
    :accessor endpoint
    :initarg :endpoint
    :documentation "A url with :<slot-name> within where the slot value is encoded.")
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
    :initform t)))

(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-send-direct (api-send-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-send-effective (api-send-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-receive-direct (api-receive-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-receive-effective (api-receive-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class api-call) (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-send-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-receive-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category &allow-other-keys)
      initargs
    (find-class (cond ((eq category :receive)
                       'api-receive-effective)
                      ((eq category :send)
                       'api-send-effective)
                      (t 'api-slot-effective)))))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category &allow-other-keys)
      initargs
    (find-class (cond ((eq category :receive)
                       'api-receive-direct)
                      ((eq category :send)
                       'api-send-direct)
                      (t 'api-slot-direct)))))

(defmethod c2mop:compute-effective-slot-definition ((class api-call) name dslots)
  (call-next-method))

(defmethod c2mop:compute-effective-slot-definition :after ((class api-call) name dslots)
  (mapc (lambda (slot)
          slot)
        dslots))

(defmethod post-process-slot ((slot api-send-direct) pkey)
  (with-slots (name name-as-string encoder)
      slot))

(defmethod post-process-slot ((slot api-receive-effective) pkey)
  (with-slots (name name-as-string)
      slot))

(defmethod c2mop:compute-slots ((class api-call))
  (call-next-method))

(defclass test ()
  ((room-id
    :category :send
    :initarg :room-id)
   (event-type
    :accessor event-type
    :name->json "eventType"
    :category :send
    :initarg :event-type)
   (txn
    :accessor txn
    :category :send)
   (event-id
    :accessor event-id
    :category :receive))
  (:metaclass api-call)
  (:endpoint "rooms/(room-id)/send/m.room.message/(txn)")
  (:rate-limited-p nil))
