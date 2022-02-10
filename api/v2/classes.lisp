(in-package #:lunamech-matrix-api/v2)

(defclass status ()
  ((latest-sync
    :accessor latest-sync
    :initarg :latest-sync
    :type (or null list))
   (next-batch
    :accessor next-batch 
    :initarg :next-batch 
    :type (or null string))))

(defun %ql (name)
  (cons name (bt:make-lock)))

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
    :type string
    :documentation "The URL of the matrix server you want to connect to")
   (api
    :accessor api
    :initarg :api
    :initform "/_matrix/client/v3/"
    :type string)
   (username
    :accessor username
    :initarg :username
    :type string)
   (txn
    :accessor txn
    :initarg :txn
    :initform 0 
    :type fixnum
    :documentation "An auto incrementing Txn id")
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
   (con-lock
    :accessor con-lock
    :initform (bt:make-lock)
    :type bt:lock
    :documentation "A lock for the connection")
   (device-id
    :accessor device-id
    :initarg :device-id
    :type string)))

(defmacro with-locked-connection ((connection) &body body)
  `(bt:with-recursive-lock-held ((con-lock ,connection))
     (locally ,@body)))

;; (slot-locks
;;  :reader slot-locks
;;  :initarg :slot-locks
;;  :initform `(,(%ql 'logged-in-p),(%ql 'filters) ,(%ql 'url),(%ql 'api),(%ql 'username)
;;              ,(%ql 'txn),(%ql 'user-id),(%ql 'password) ,(%ql 'auth) ,(%ql 'encryption)
;;              ,(%ql 'encryption) ,(%ql 'device-id))
;;  :documentation "A list of the locks for each slot.")))

;; (defmethod c2mop:slot-value-using-class :around ((class connection) object slot)
;;   (let* ((name (c2mop:slot-definition-name slot))
;;          (lock (cdr (assoc name (slot-locks object)))))
;;     (print "locking")
;;     (bt:with-lock-held (lock)
;;       (call-next-method))))

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
    :initarg :next-sync-string
    :type (or null string)))
  (:documentation "Used to store data about a filter and its key"))


