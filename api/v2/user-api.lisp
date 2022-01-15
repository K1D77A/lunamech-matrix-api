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
        (with-hash-keys (|access_token| |device_id| |user_id|)
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

(defun public-rooms (connection &rest rest &key &allow-other-keys)
  (call-api (apply #'make-instance 'rooms%public-rooms
                   (append (list :connection connection)
                           rest))))

(defun get-room-state (connection room-id &rest rest &key &allow-other-keys)
  (call-api (apply #'make-instance 'events%get-state-events-in-room
                   (append (list :connection connection
                                 :room-id room-id)
                           rest))))

(defun join-room (connection room-id)
  "Makes CONNECTION joined the room denoted by ROOM-ID. Assuming it can."
  (call-api (make-instance 'rooms%join-a-room :room-id room-id :connection connection)))

(defun leave-room (connection room-id)
  "Makes CONNECTION leave the room denoted by ROOM-ID."
  (call-api (make-instance 'rooms%leave-a-room :room-id room-id :connection connection)))

(defun joined-rooms (connection)
  "Returns the rooms that CONNECTION is within."
  (call-api (make-instance 'rooms%my-joined-rooms :connection connection)))

(defun send-message-to-room (connection room-id message)
  (multiple-value-bind (hash type)
      (object%event/m-room-message/m-text%basic message)
    (send-event-to-room connection room-id type hash)))

(defun send-message-event-to-room (connection room-id message-event)
  (send-event-to-room connection room-id "m.room.message" message-event))

(defun send-event-to-room (connection room-id event-type event)
  (call-api
   (make-instance 'events%put-message-event-into-room
                  :body event 
                  :room-id room-id
                  :event-type event-type
                  :connection connection)))

(defun send-state-event-to-room (connection room-id event-type event)
  (call-api
   (make-instance 'events%put-state-event-into-room
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

(defun kick-user-from-room (connection room-id user-id &optional (reason-why "kicked"))
  "Kicks the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (call-api (make-instance 'rooms%kick-user-from-room
                           :reason reason-why
                           :user-id user-id
                           :connection connection
                           :room-id room-id)))

(defun user-display-name (connection user-id)
  (call-api (make-instance 'profile%get-display-name
                           :user-id user-id
                           :connection connection)))

(defun valid-user-p (connection user-id)
  (handler-case
      (user-display-name connection user-id)
    (M-NOT-FOUND ()
      nil)
    (M-INVALID-PARAM ()
      nil)
    (M-UNKNOWN ()
      nil)))

(defun ban-user-from-room (connection room-id user-id &optional (reason-why "banned"))
  "Bans the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (call-api (make-instance 'rooms%ban-user-from-room
                           :reason reason-why
                           :user-id user-id
                           :connection connection
                           :room-id room-id)))

(defun unban-user-from-room (connection room-id user-id)
  "Unbans the user denoted by USER-ID from ROOM-ID."
  (call-api (make-instance 'rooms%unban-user-from-room
                           :user-id user-id
                           :connection connection
                           :room-id room-id)))

(defun members-in-room (connection room-id &rest keys &key &allow-other-keys)
  "Gets the members of ROOM-ID."
  (call-api (apply #'make-instance 'events%get-room-members
                   (append (list :room-id room-id
                                 :connection connection)
                           keys))))

(defun members-in-room%ids (connection room-id)
  (let ((members (members-in-room connection room-id)))
    (mapcar (lambda (ele)
              (gethash "user_id" ele))
            (gethash "chunk" members))))

(defun members-in-room-ids (connection room-id)
  "Gets the members id's of ROOM-ID."
  (call-api (make-instance 'events%get-joined-members
                           :room-id room-id
                           :connection connection)))

(defun upload-content (connection filename content-type content-bytes)
  (call-api
   (make-instance 'media%upload
                  :connection connection
                  :filename filename
                  :content-type content-type
                  :bytes content-bytes)))

(defun send-image-file-to-room (connection room-id name content-type path
                                &rest keys &key &allow-other-keys)
  "Uploads image from PATH to to ROOM-ID. Keys are passed to 
object%event/m-room-message/m-image"
  (let* ((file (alexandria:read-file-into-byte-vector path))
         (url (gethash "content_uri" (upload-content connection name content-type file))))
    (send-message-event-to-room connection room-id
                                (apply #'object%event/m-room-message/m-image
                                       (append (list :body name
                                                     :url url)
                                               keys)))))

(defun send-image-bytes-to-room (connection room-id name content-type bytes
                                 &rest keys &key &allow-other-keys)
  "Uploads BYTES from BYTES to to ROOM-ID. Keys are passed to 
object%event/m-room-message/m-image"
  (let ((url (gethash "content_uri" (upload-content connection name content-type bytes))))
    (send-message-event-to-room connection room-id
                                (apply #'object%event/m-room-message/m-image
                                       (append (list :body name
                                                     :url url)
                                               keys)))))


(defun messages-in-room (connection room-id &rest keys &key &allow-other-keys)
  (call-api (apply #'make-instance 'events%get-room-messages 
                   (append (list :connection connection :room-id room-id)
                           keys))))

(defun invite-member-to-room (connection user-id room-id)
  (call-api (make-instance 'rooms%invite-user-to-room
                           :connection connection
                           :user-id user-id
                           :room-id room-id)))

(defun create-room (connection name room-alias topic
                    &rest keys &key &allow-other-keys)
  (call-api (apply #'make-instance 'create-room
                   (append (list :name name :room-alias room-alias
                                 :topic topic :connection connection
                                 :preset "public_chat" :visibiilty "public")
                           keys))))

(defun create-private-room (connection invite 
                            &rest keys &key &allow-other-keys)
  (call-api (apply #'make-instance 'create-room
                   (append (list 
                            :connection connection
                            :invite invite
                            :is-direct t 
                            :creation-content
                            (%quick-hash `(("m.federate" . t)))
                            :visibility "private" :preset "private_chat")
                           keys))))

(defun user-profile-url (connection user-id)
  (call-api (make-instance 'profile%get-avatar-url
                           :user-id user-id
                           :connection connection)))


(defun download-content (connection mxc-address &key (allow-remote "true"))
  (let* ((mxc-list (str:split ":" mxc-address))
         (content-id (second mxc-list))
         (homeserver (second mxc-list)))
    (call-api
     (make-instance 'media%get-media
                    :media-id content-id
                    :connection connection
                    :server-name homeserver
                    :allow-remote allow-remote))))

(defun add-to-account-data (connection user-id key data)
  "Add DATA (a hashtable) to the type KEY for USER-ID."
  (call-api (make-instance 'account-data%set-data
                           :user-id user-id
                           :connection connection
                           :data-type key
                           :body data)))

(defun get-account-data (connection user-id event-type)
  (call-api (make-instance 'account-data%get-data
                           :user-id user-id
                           :connection connection
                           :data-type event-type)))

(defun get-user-presence (connection user-id)
  (call-api (make-instance 'presence%get-presence
                           :connection connection
                           :user-id user-id)))

(defun user-online-p (connection user-id)
  (getf (get-user-presence connection user-id) :|currently_active|))

(defun set-avatar-url (connection user-id mxc)
  (call-api (make-instance 'profile%set-avatar-url
                           :avatar-url mxc
                           :user-id user-id
                           :connection connection)))

(defun request-open-id-token (connection)
  (call-api (make-instance 'openid%request-openid
                           :connection connection
                           :user-id (user-id connection))))






