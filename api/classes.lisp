(in-package #:lunamech-matrix-api)

(defclass status ()
  ((latest-sync
    :accessor latest-sync
    :initarg :latest-sync)))

(defclass connection ()
  ((logged-in-p
    :initform nil
    :initarg :logged-in-p
    :accessor logged-in-p)
   (filters
    :initarg :filters
    :initform nil 
    :accessor filters)
   (status
    :accessor status
    :initarg :status
    :initform (make-instance 'status))
   (url
    :accessor url
    :initarg :url
    :type string)
   (api
    :accessor api
    :initarg :api
    :type string)
   (username
    :accessor username
    :initarg :username
    :type string)
   (user-id
    :accessor user-id
    :initarg :user-id
    :type string)
   (password
    :accessor password
    :type string
    :initarg :password)
   (auth
    :accessor auth
    :initarg :auth
    :type auth)
   (encryption
    :accessor encryption
    :initarg :encryption 
    :type encryption
    :documentation "The slot used to store the associated encryption object")
   (device-id
    :accessor device-id
    :initarg :device-id
    :type string)))

(defmethod jojo:%to-json ((obj connection))
  (with-slots (username user-id auth device-id api url filters)
      obj
    (jojo:with-object 
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "user-id" user-id)
      (jojo:write-key-value "device-id" device-id)
      (jojo:write-key-value "api" api)
      (jojo:write-key-value "url" url)
      (jojo:write-key-value "auth" (jojo:%to-json auth))
      (jojo:write-key-value "filters"                              
                            (mapc #'jojo:%to-json filters)))))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (format
     stream "~&URL: ~S~%Username: ~S~%Logged in: ~S~%Auth: ~S~%Device-id: ~S~%"
     (str:concat (url connection) (api connection))
     (username connection)
     (logged-in-p connection)
     (if (slot-boundp connection 'auth)
         (auth connection)
         "Not authorized yet")
     (if (slot-boundp connection 'device-id)
         (device-id connection)
         "No device ID yet"))))

(defmethod homeserver ((con connection))
  (second (str:split #\: (user-id con))))

(defun make-connection (username password url api)
  (make-instance 'connection :username username :password password
                             :url url :api api))

(defclass encryption ()
  ((olm-account
    :accessor olm-account
    :initarg :olm-account)
   (server-otk
    :accessor server-otk
    :initarg :server-otk)))

(defclass auth ()
  ((token
    :accessor token
    :initarg :token
    :type string)))

(defmethod jojo:%to-json ((obj auth))
  (jojo:with-object
    (jojo:write-key-value "token" (slot-value obj 'token))))

(defclass filter ()
  ((key
    :accessor key
    :initarg :key
    :type keyword)
   (id
    :accessor id
    :initarg :id
    :type (or string integer))
   (last-sync-string
    :accessor last-sync-string
    :initarg :last-sync-string
    :type (or null string))
   (next-sync-string
    :accessor next-sync-string
    :initarg :next-sync-string
    :type (or null string)))
  (:documentation "Used to store data about a filter and its key"))

(defmethod jojo:%to-json ((obj filter))
  (with-slots (key id)
      obj 
    (jojo:with-object
      (jojo:write-key-value "key" key)
      (jojo:write-key-value "id" id)
      (jojo:write-key-value "last-sync-string" (if (slot-boundp obj 'last-sync-string)
                                                   (slot-value obj 'last-sync-string)
                                                   "null"))
      (jojo:write-key-value "next-sync-string" (if (slot-boundp obj 'next-sync-string)
                                                   (slot-value obj 'next-sync-string)
                                                   "null")))))

(defun serialize-connection (connection
                             &optional (slots '(url username logged-in-p auth
                                                device-id status
                                                api filters status user-id)))
  (flet ((when-boundp (slot)
           (let ((key (intern (string-upcase slot) :keyword)))
             (if (slot-boundp connection slot)
                 (list key (serialize-object (slot-value connection slot)))
                 (list key :no-val)))))        
    (reduce #'append
            (mapcar #'when-boundp slots))))

(defmethod serialize-object (obj)
  obj)

(defmethod serialize-object ((obj auth))
  (list :token
        (if (slot-boundp obj 'token)
            (slot-value obj 'token)
            :no-val)))

(defmethod serialize-object ((obj list))
  (mapcar #'serialize-object obj))

(defmethod serialize-object ((obj filter))
  (with-slots (key id)
      obj
    (list :key key :id id
          :last-sync-string (if (slot-boundp obj 'last-sync-string)
                                (slot-value obj 'last-sync-string)
                                :no-val)
          :next-sync-string (if (slot-boundp obj 'next-sync-string)
                                (slot-value obj 'next-sync-string)
                                :no-val))))

(defmethod serialize-object ((obj status))
  (list :latest-sync
        (if (slot-boundp obj 'latest-sync)
            (slot-value obj 'latest-sync)
            :no-val)))

(defmethod restore-from-key ((key (eql :status)) list)
  (apply 'make-instance 'status list))

(defmethod restore-from-key ((key (eql :auth)) list)
  (apply 'make-instance 'auth list))

(defmethod restore-from-key ((key (eql :filters)) list)
  (mapcar (lambda (lst)
            (restore-from-key :filter lst))
          list))

(defmethod restore-from-key ((key (eql :filter)) list)
  (apply 'make-instance 'filter list))

(defmethod restore-from-key :around (key list)
  (let ((res (call-next-method)))
    (when (typep res 'c2mop:standard-object)
      (let ((slots (c2mop:class-direct-slots (class-of res))))
        (mapc (lambda (slot)
                (let* ((slot-name (c2mop:slot-definition-name slot))
                       (val (slot-value res slot-name)))
                  (when (eq val :no-val)
                    (slot-makunbound res slot-name))))
              slots)))
    res))

(defmethod restore-from-key (key list)
  list)

(defun restore-connection (list &optional
                                  (slots '(url username logged-in-p auth
                                           device-id status
                                           api filters status user-id)))
  (let ((connection (make-instance 'connection)))
    (dolist (slot slots connection)
      (let* ((slot-key (intern (string-upcase slot) :keyword))
             (val (getf list slot-key)))
        (setf (slot-value connection slot)
              (restore-from-key slot-key val))))))
      



