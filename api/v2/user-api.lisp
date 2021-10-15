(in-package #:lunamech-matrix-api/v2)


(defun password-login (connection)
  (with-accessors ((username username)
                   (password password)
                   (user-id user-id)
                   (device-id device-id)
                   (logged-in-p logged-in-p)
                   (auth auth))
      connection 
    (let ((call (make-instance 'lunamech-matrix-api/v2/api:login-connection 
                               :connection connection
                               :device-id (if (slot-boundp connection 'device-id)
                                              device-id
                                              nil)
                               :identifier (object%identifier-type/m-id-user username)
                               :initial-device-display-name username
                               :password password)))
      (print call)
      (break)
      (destructuring-bind (&key |access_token| |device_id| |user_id| &allow-other-keys)
          (call-api call)
        (when |device_id|
          (setf device-id |device_id|))
        (setf auth (make-instance 'auth :token |access_token|)
              user-id |user_id|
              logged-in-p t))
      connection)))

(defun logout (connection)
  (with-accessors ((logged-in-p logged-in-p)
                   (auth auth))
      connection
    (let ((call (make-instance 'lunamech-matrix-api/v2/api:logout-connection
                               :connection connection)))
      (call-api call)
      (setf logged-in-p nil)
      (slot-makunbound connection 'auth))
    connection))

(defun send-message-to-room (connection room-id message)
  (make-instance 'lunamech-matrix-api/v2/api:events%put-message-event-into-room
                 :body
                 (%quick-hash
                  `(("msgtype" . "m.text")
                    ("body" . ,message)))
                 :room-id room-id
                 :txn (random 34857245)
                 :event-type "m.room.message" :connection connection))

(defun send-event-to-room (connection room-id event-type event)
  (make-instance 'lunamech-matrix-api/v2/api:events%put-message-event-into-room
                 :body event 
                 :room-id room-id
                 :txn (random 34857245)
                 :event-type event-type
                 :connection connection))

(defun redact-event-in-room (connection room-id event-id reason)
  (make-instance 'lunamech-matrix-api/v2/api:events%redact-event
                 :reason reason
                 :room-id room-id
                 :event-id event-id
                 :txn (random 1383574)
                 :connection connection))
      
  





