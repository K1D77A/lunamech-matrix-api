(in-package #:lunamech-matrix-api/v2)


(defun password-login (connection)
  (with-accessors ((username username)
                   (password password)
                   (user-id user-id)
                   (device-id device-id)
                   (logged-in-p logged-in-p)
                   (auth auth))
      connection 
    (let ((call (make-instance 'login-connection 
                               :connection connection
                               :device-id (if (slot-boundp connection 'device-id)
                                              device-id
                                              nil)
                               :identifier (object%identifier-type/m-id-user username)
                               :initial-device-display-name username
                               :password password)))
      (with-locked-connection (connection)
        (destructuring-bind (&key |access_token| |device_id| |user_id| &allow-other-keys)
            (call-api call)
          (when |device_id|
            (setf device-id |device_id|))
          (setf auth (make-instance 'auth :token |access_token|)
                user-id |user_id|
                logged-in-p t))))
    connection))

(defun logout (connection)
  (with-accessors ((logged-in-p logged-in-p)
                   (auth auth))
      connection
    (with-locked-connection (connection)
      (let ((call (make-instance 'logout-connection
                                 :connection connection)))
        (call-api call)
        (setf logged-in-p nil)
        (slot-makunbound connection 'auth)))
    connection))

(defun public-rooms (connection)
  (call-api (make-instance 'rooms%public-rooms :connection connection)))

(defun send-message-to-room (connection room-id message)
  (multiple-value-bind (hash type)
      (object%event/m-room-message message)
    (send-event-to-room connection room-id type hash)))

(defun send-event-to-room (connection room-id event-type event)
  (call-api (make-instance 'events%put-message-event-into-room
                           :body event 
                           :room-id room-id
                           :event-type event-type
                           :connection connection)))

(defun redact-event-in-room (connection room-id event-id reason)
  (call-api (make-instance 'events%redact-event
                           :reason reason
                           :room-id room-id
                           :event-id event-id
                           :connection connection)))







