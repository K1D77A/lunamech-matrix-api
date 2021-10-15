(in-package #:lunamech-matrix-api)


(defun password-login-plist (connection)
  (let ((idp (slot-boundp connection  'device-id))
        (list 
          (list :|type| "m.login.password"
                :|identifier| (list :|type| "m.id.user"
                                    :|user| (username connection))
                :|password| (password connection))))
    (if idp
        (nconc list (list :|device_id| (device-id connection)))
        list)))

(defun password-login (connection)
  (auth-req (:post-no-auth connection ("login")
             (password-login-plist connection) resp)
    (psetf (logged-in-p connection) t
           (auth connection)
           (make-instance 'auth :token (getf resp :|access_token|))
           (device-id connection) (getf resp :|device_id|)
           (user-id connection) (getf resp :|user_id|))
    resp))

(defun public-rooms (connection)
  "Returns all the public rooms accessible by CONNECTION."
  (auth-req (:get connection ("publicRooms") nil resp)
    resp))

(defun get-room-state (connection room-id )
  (auth-req (:get connection ("rooms/" (url-e room-id) "/state")
             nil resp)
    resp))

(defun get-room-events (connection room-id event-type)
  (auth-req (:get connection ("rooms/" (url-e room-id) "/state/" event-type)
             nil resp)
    resp))

(defun join-room (connection id)
  "Makes CONNECTION joined the room denoted by ID. Assuming it can."
  (auth-req (:post connection ("join/" id) (list :roomid id) resp)
    resp))

(defun leave-room (connection id)
  "Makes CONNECTION leave the room denoted by ID."
  (auth-req (:post connection ("rooms/" id "/leave") nil resp)
    resp))

(defun joined-rooms (connection)
  "Returns the rooms that CONNECTION is within."
  (auth-req (:get connection ("joined_rooms") nil resp)
    resp))

;; (defun room-aliases (connection room-id)
;;   (dex:get "https://matrix.scyldings.com/_matrix/client/unstable/org.matrix.msc2432/rooms/%21WJvFXSrAnfoqNgwqpE%3Ascyldings.com/aliases"
;;            :headers (gen-headers connection)))

(defun send-message-event-to-room (connection room-id message-event)
  "Sends the message MESSAGE-EVENT to the ROOM-ID, assuming CONNECTION is within it."
  (auth-req (:put-event connection
             ("rooms/" (url-e room-id) "/send/m.room.message/"
                       (format nil "~D" (random 1000000000000000)))
             (list :object message-event)
             resp)
    resp))

(defun send-event-to-room (connection room-id event-type message-event)
  "Sends the event to the ROOM-ID, assuming CONNECTION is within it."
  (auth-req (:put-event connection
             ("rooms/" (url-e room-id) "/send/" event-type "/"
                       (format nil "~D" (random 100000000)))
             (list :object message-event)
             resp)
    resp))

(defun send-state-event-to-room (connection room-id event-type event
                                 &optional (state-key ""))
  (auth-req (:put-event connection
             ("rooms/" (url-e room-id) "/state/" event-type "/" state-key)
             (list :object event)
             resp)
    resp))


(defun make-auth (connection)
  "Creates a plist which represents an auth token that can be sent to the server using data within
CONNECTION."
  (list :|auth| (list :|access_token| (token (auth connection)))))

(defun logout (connection)
  "Logs out CONNECTION."
  (auth-req (:post connection ("logout") nil resp)
    resp
    (setf (logged-in-p connection) nil)))

(defun send-message-to-room (connection room-id message)
  "Sends the message MESSAGE to the ROOM-ID, assuming CONNECTION is within it."
  (auth-req (:post connection ("rooms/" (url-e room-id) "/send/m.room.message")
             (list :|msgtype| "m.text" :|body| message) resp)
    resp))

(defun kick-user-from-room (connection room-id user-id
                            &optional (reason-why "kicked"))
  "Kicks the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (auth-req (:post connection ("rooms/" (url-e room-id) "/kick")
             (list :|user_id| user-id :|reason| reason-why) resp)
    resp))

(defun user-display-name (connection user-id)
  (auth-req (:get connection ("profile/" (url-e user-id) "/displayname")
             nil resp)
    resp))

(defun valid-user-p (connection user-id)
  (handler-case
      (user-display-name connection user-id)
    (M-NOT-FOUND ()
      nil)
    (M-INVALID-PARAM ()
      nil)
    (M-UNKNOWN ()
      nil)))

(defun ban-user-from-room (connection room-id user-id &optional
                                                        (reason-why "banned"))
  "Bans the user denoted by USER-ID from ROOM-ID with the REASON-WHY."
  (auth-req (:post connection ("rooms/" (url-e room-id) "/ban")
             (list :|user_id| user-id :|reason| reason-why) resp)
    resp))

(defun unban-user-from-room (connection room-id user-id)
  "Unbans the user denoted by USER-ID from ROOM-ID."
  (auth-req (:post connection ("rooms/" room-id "/unban")
             (list :|user_id| user-id) resp)
    resp))

(defun members-in-room (connection room-id)
  "Gets the members of ROOM-ID."
  (auth-req (:get connection ("rooms/" (url-e room-id) "/members")
             nil resp)
    resp))

(defun members-in-room%ids (connection room-id)
  (let ((members (members-in-room connection room-id)))
    (mapcar (lambda (ele)
              (getf ele :|user_id|))
            (getf members :|chunk|))))

(defun members-in-room-ids (connection room-id)
  "Gets the members id's of ROOM-ID."
  (auth-req (:get connection ("rooms/" (url-e room-id) "/joined_members")
             nil resp)
    resp))

(defun admin-whois (connection user-id)
  "Performs a whois call on USER-ID, only an admin can call this on non self users."
  (auth-req (:post connection ("/admin/whois/" user-id)
             nil resp)
    resp))

(defun upload-content (connection filename content-type content-bytes)
  (auth-req (:post-content connection ("upload?filename="
                                       filename "/"
                                       (format nil "~D" (random 100000)))
             ;;passing the other values requires for (post-content .. )
             ;;in the plist
             (list :content-type content-type :content content-bytes)
             resp :media)
    resp))

(defun send-image-file-to-room (connection room-id name content-type path)
  ""
  (let* ((file (alexandria:read-file-into-byte-vector path))
         (url (getf (upload-content connection name content-type file)
                    :|content_uri|)))
    (send-message-event-to-room connection room-id
                                (make-instance 'm-image :body name :url url))))

(defun send-image-bytes-to-room (connection room-id name content-type bytes)
  ""
  (let ((url (getf (upload-content connection name content-type bytes)
                   :|content_uri|)))
    (send-message-event-to-room connection room-id
                                (make-instance 'm-image :body name :url url))))

(defun send-file-bytes-to-room (connection room-id name bytes file-info-object)
  ""
  (let ((url (getf (upload-content connection name (mimetype file-info-object) bytes)
                   :|content_uri|)))
    (send-message-event-to-room connection room-id
                                (make-instance 'm-file :body name :url url
                                                       :info file-info-object))))

(defun downcase-symbols (list)
  (loop :for ele :in list
        :collect (if (symbolp ele)
                     (string-downcase ele)
                     ele)))

(defun messages-in-room (connection room-id
                         &rest keys &key from to dir limit filter)
  (declare (ignorable from to dir limit filter))
  (auth-req (:get connection ("rooms/" (url-e room-id) "/messages")
                  (downcase-symbols keys) resp)
    resp))

(defun invite-member-to-room (connection user-id room-id)
  (auth-req (:post connection ("rooms/" (url-e room-id) "/invite")
                   (list :|user_id| user-id) resp)
    resp))

(defun events-in-room (connection room-id from &optional (timeout 1000))
  (auth-req (:get connection ("events")
             (list :|room_id| room-id
                   :|from| from
                   :|timeout| timeout)
             resp)
    resp))

(defun create-room (connection name room-alias topic
                    &key (private t) (invite nil))
  (check-type invite (or list null))
  (auth-req (:post connection ("createRoom")
             (list :|preset| (if private
                                 "private_chat"
                                 "public_chat")
                   :|visibility| (if private
                                     "private"
                                     "public")
                   :|invite| invite
                   :|room_alias_name| room-alias
                   :|name| name
                   :|topic| topic
                   :|creation_content| (list :|m.federate| t))
             resp)
    resp))

(defun create-private-room (connection invite)
  (check-type invite list)
  (auth-req (:post connection ("createRoom")
             (list :|preset| "private_chat"
                   :|visibility| "private"
                   :|invite| invite
                   :|creation_content| (list :|m.federate| t)
                   :|is_direct| t)
             resp)
    resp))

(defun user-profile-url (connection user-id)
  (auth-req (:get connection ("profile/" (url-e user-id) "/avatar_url")
             nil resp)
    resp))

(defun download-content (connection mxc-address)
  (let* ((mxc-list (str:split "/" mxc-address))
         (content-id (first (last mxc-list)))
         (homeserver (third mxc-list)))
    (auth-req (:content-get connection ("download/" (url-e homeserver) "/"
                                                    content-id)
               nil resp :media)
      resp)))

(defun add-to-account-data (connection user-id key data)
  (auth-req (:put-no-json connection
             ("user/" (url-e user-id) "/account_data/" key)
             data resp)
    resp))

(defun get-account-data (connection user-id event-type)
  (check-type event-type string)
  (auth-req (:get connection
             ("user/" (url-e user-id) "/account_data/" event-type)
             nil resp)
    resp))

(defun get-user-presence (connection user-id)
  (auth-req (:get connection ("presence/" (url-e user-id) "/status") nil resp)
    resp))

(defun user-online-p (connection user-id)
  (getf (get-user-presence connection user-id) :|currently_active|))

(defun set-avatar-url (connection user-id mxc)
  (auth-req (:put connection ("profile/" (url-e user-id) "/avatar_url")
             (list :|avatar_url| mxc)
             resp)
    resp))

(defun request-open-id-token (connection)
  (auth-req (:post-object connection ("user/" (url-e (user-id connection))
                                              "/openid/request_token")
             (jojo:to-json (make-instance 'empty-object)) resp)
    resp))
