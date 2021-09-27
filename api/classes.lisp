(in-package #:lunamech-matrix-api)

(defclass status ()
  ((latest-sync
    :accessor latest-sync)))

(defclass connection ()
  ((logged-in-p
    :initform nil
    :accessor logged-in-p)
   (filters
    :initarg :filters
    :initform nil 
    :accessor filters)
   (status
    :accessor status
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
    :type string
    :initarg :username)
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
    :type auth
    :initarg :auth)
   (encryption
    :accessor encryption
    :type encryption
    :documentation "The slot used to store the associated encryption object")
   (device-id
    :accessor device-id
    :type string)))

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
    :type (or null string)))
  (:documentation "Used to store data about a filter and its key"))
