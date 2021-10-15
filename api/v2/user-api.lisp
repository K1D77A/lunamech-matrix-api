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
      (destructuring-bind (&key |access_token| |device_id| |user_id| &allow-other-keys)
          (call-api call)
        (when |device_id|
          (setf device-id |device_id|))
        (setf auth (make-instance 'auth :token |access_token|)
              user-id |user_id|
              logged-in-p t))
      connection)))

(defun logout (connection)
  (with-accessors ((username username)
                   (password password)
                   (user-id user-id)
                   (device-id device-id)
                   (logged-in-p logged-in-p)
                   (auth auth))
      connection
    (let ((call (make-instance 'lunamech-matrix-api/v2/api:logout-connection
                               :connection connection)))
      (call-api call)
      (setf logged-in-p nil)
      (slot-makunbound connection 'auth))
    connection))

(defun send-message-to-room )
