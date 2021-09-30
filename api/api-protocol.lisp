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

(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-send-direct (api-send-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-send-effective (api-send-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-send-special-direct (api-send-slot-special
                                   c2mop:standard-direct-slot-definition)
  ())

(defclass api-send-special-effective (api-send-slot-special
                                      c2mop:standard-effective-slot-definition)
  ())


(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-send-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-send-slot-special)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (cond ((eq category :send)
                       (if specialp
                           'api-send-special-effective
                           'api-send-effective))
                      (t 'api-slot-effective)))))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (cond ((eq category :send)
                       (if specialp
                           'api-send-special-direct
                           'api-send-direct))
                      (t 'api-slot-direct)))))

(defmethod c2mop:compute-effective-slot-definition ((class api-call) name dslots)
  (call-next-method))

(defmethod c2mop:compute-slots ((class api-call))
  (with-slots (string-constructor endpoint required-slots special-slot)
      class
    (multiple-value-bind (fun slots)
        (compose-string-into-lambda class (in-list endpoint))
      (setf string-constructor fun
            required-slots slots
            special-slot (find-special-slot class))
      (call-next-method))))

(defmethod find-special-slot ((class api-call))
  (with-slots (special-slot specialp)
      class
    (when (first specialp)
      (let* ((slots (c2mop:class-direct-slots class))
             (slot (first (remove-if-not  (lambda (slot) (typep slot 'api-send-special-direct))
                                          slots))))
        (if slot
            (c2mop:slot-definition-name slot)
            (error
             "Class is set to special but you have no slots that are declared special"))))))

(defclass api ()
  ((connection
    :reader connection
    :initarg :connection)
   (result
    :accessor result))
  (:metaclass api-call))

(defun url-e (url)
  (do-urlencode:urlencode url))


(defmethod initialize-instance :after ((class api) &rest initargs)
  (with-slots (string-constructor)
      (class-of class)
    (c2mop:set-funcallable-instance-function class (lambda () (funcall #'call-api class)))))
;;(call-next-method)))

(defun compose-string-into-lambda (class string)
  "Takes a string like 'foo/:bar/quux and returns a function with N arguments where 
N is the number of words prefixed with :, so in the example it would be a function with 
one argument bar. When executed with this argument :bar would be replaced and a new 
string returned."
  (if (some #'in-url-p (c2mop:class-direct-slots class))
      (let* ((split (str:split #\/ string))
             (class-slots (c2mop:class-direct-slots class))
             (replaced-with-syms (mapcar (lambda (string)
                                           (if (str:starts-with-p ":" string)
                                               (intern (string-upcase (subseq string 1)))
                                               string))
                                         split))
             (lambda-list (remove-if-not #'symbolp replaced-with-syms))
             (filtered-slots (remove-if
                              #'null
                              (mapcar (lambda (sym)
                                        (find sym class-slots
                                              :key #'c2mop:slot-definition-name))
                                      lambda-list)))
             (encoders (loop :for slot :in filtered-slots
                             :collect (list :name (c2mop:slot-definition-name slot)
                                            :encoder (slot-value slot 'encoder))))
             (optional-slots (remove-if #'requiredp filtered-slots))
             (required-slots (remove-if-not #'requiredp filtered-slots
                                            :key ))
             (o-names (mapcar #'c2mop:slot-definition-name optional-slots))
             (r-names (mapcar #'c2mop:slot-definition-name required-slots))
             (final-lambda-list (append r-names (if o-names
                                                    '(&optional)
                                                    nil)
                                        (mapcar (lambda (name)
                                                  (list name nil))
                                                o-names))))
        (print lambda-list)
        (print filtered-slots )
        (print replaced-with-syms)
        (values     
         (compile nil
                  `(lambda ,final-lambda-list
                     (format nil "~{~A~^/~}"
                             (remove-if #'null (list ,@replaced-with-syms)))))
         lambda-list))
      (lambda ()
        string)))

(defclass message-room (api)
  ((room-id
    :category :send
    :initarg :room-id
    :encoder #'url-e
    :in-url-p t)
   (event-type
    :accessor event-type
    :category :send
    :in-url-p t
    :encoder nil
    :initarg :event-type
    :encoder nil)
   (body 
    :accessor body
    :initarg :body
    :category :send
    :specialp t)
   (txn
    :accessor txn
    :category :send
    :requiredp nil
    :encoder nil
    :in-url-p t))
  (:metaclass api-call)
  (:endpoint "rooms/:room-id/send/:event-type/:txn")
  (:request-type :post)
  (:requires-auth-p nil)
  (:rate-limited-p nil)
  (:specialp t))

(defmacro defapi (name slots &rest class-options)
  `(progn (defclass ,name (api)
            ,slots
            ,@class-options)))

(defapi login 
        ((login-type
          :accessor login-type
          :category :send
          :initform "m.login.password"
          :name->json :type
          :requiredp t)
         (identifier
          :accessor identifier
          :initarg :identifier
          :category :send)
         (password
          :accessor password
          :initarg :password
          :category :send)
         (device-id
          :accessor device-id
          :initarg :device-id
          :category :send
          :requiredp nil))
        (:metaclass api-call)
        (:endpoint "login")
        (:request-type :post)
        (:requires-auth-p nil))

(defapi logout
        ()
        (:metaclass api-call)
        (:rate-limited-p nil)
        (:endpoint "logout"))

(defmethod validate-slot-for-sending ((api api) slot)
  (with-accessors ((requiredp requiredp))
      slot
    (when (and requiredp (not (slot-boundp api (c2mop:slot-definition-name slot))))
      (error "Trying to send a request but missing data from required slot."))))

(defmethod slots-to-send ((api api))
  (let ((slots (c2mop:class-direct-slots (class-of api))))
    (remove-if-not (lambda (slot)
                     (eq (category slot) :send))
                   slots)))

(defmethod validate-slots-for-sending ((api api) slots-to-send)
  (mapc (lambda (slot)
          (validate-slot-for-sending api slot))
        slots-to-send))

(defmethod remove-unbound-slots ((api api) slots-to-send)
  (remove-if-not (lambda (slot) (slot-boundp api (c2mop:slot-definition-name slot)))
                 slots-to-send))

(defmethod slots-to-send-and-validated ((api api))
  (remove-unbound-slots api (validate-slots-for-sending api (slots-to-send api))))

(defmethod slot->json ((api api) slot)
  (let ((name (c2mop:slot-definition-name slot)))
    (cl-json:encode-object-member 
     (or (and (slot-boundp slot 'name->json)(slot-value slot 'name->json))
         (str:snake-case (string name)))
     (slot-value api name))))

(defmethod to-json ((api api))
  (let* ((slots (slots-to-send-and-validated api))
         (stream (make-string-output-stream))
         (cl-json:*json-output* stream))
    (cl-json:with-object ()
      (mapc (lambda (s) (slot->json api s)) slots))
    (get-output-stream-string stream)))

(defun in-list (e)
  (if (listp e)
      (first e)
      e))

(defmethod string-constructor ((api api))
  (string-constructor (class-of api)))

(defmethod request-type ((api api))
  (in-list (request-type (class-of api))))

(defmethod rate-limited-p ((api api))
  (in-list (rate-limited-p (class-of api))))

(defmethod api ((api api))
  (in-list (api (class-of api))))

(defmethod content-type ((api api))
  (content-type (class-of api)))

(defmethod requires-auth-p ((api api))
  (in-list (requires-auth-p (class-of api))))

(defmethod determine-request-fun ((key (eql :post)))
  #'dexador:post)

(defmethod determine-request-fun ((key (eql :get)))
  #'dexador:get)

(defmethod specialp ((api api))
  (in-list (specialp (class-of api))))

(defmethod required-slots ((api api))
  (required-slots (class-of api)))

(defmethod special-slot ((api api))
  (special-slot (class-of api)))

(defmethod values-for-required ((api api))
  (let ((req (required-slots api)))
    (loop :for name :in req
          :when (slot-boundp api name)
            :collect (slot-value api name))))

(defmethod generate-authorization ((api api))
  (with-accessors ((connection connection))
      api
    (let* ((auth (auth connection))
           (token (token auth)))
      (format nil "Bearer ~A" token))))

(defmethod generate-content-type ((api api))
  (content-type api))

(defmethod generate-headers ((api api))
  (cons (cons "Content-Type"  (generate-content-type api))
        (if (requires-auth-p api)
            (cons (cons "Authorization" (generate-authorization api))
                  nil))))

(defmethod generate-body%special ((api api))
  (with-accessors ((special-slot special-slot))
      api 
    (unless (slot-boundp api special-slot)
      (error "You have declared a slot special but it is not bound..."))
    (jojo:to-json (slot-value api special-slot))))

(defmethod generate-body%normal ((api api))
  (to-json api))

(defmethod generate-body ((api api))
  (if (specialp api)
      (generate-body%special api)
      (generate-body%normal api)))

(defmethod call-api ((api api))
  (symbol-macrolet ((string-constructor (string-constructor api))
                    (request-type (request-type api))
                    (connection (connection api)))
    (let* ((end (apply string-constructor (values-for-required api)))
           (fun (determine-request-fun request-type))
           (api-string (api api)))
      (with-accessors ((url url)
                       (auth auth))
          connection
        (let ((headers (generate-headers api))
              (content (to-json api))
              (url (concatenate 'string url api-string end)))
          (let ((res
                  (execute-api-call api fun url 
                                    `(:headers ,headers :content ,content
                                      :use-connection-pool nil))))
            (setf (result api) (jojo:parse res))
            api))))))

(defmethod execute-api-call ((api api) fun url args-plist)
  (with-captured-dex-error
    (apply fun url args-plist)))
