;; (defpackage #:lunamech-matrix-api/v2/10
;;   (:use #:lunamech-matrix-api/v2 #:cl)
;;   (:export #:create-room
;;            #:visibility
;;            #:room-alias-name
;;            #:name
;;            #:topic
;;            #:invite
;;            #:invite-3pid
;;            #:room-version
;;            #:creation-content
;;            #:initial-state
;;            #:preset
;;            #:is-direct
;;            #:power-level-content-override

;;            #:alias%new-room-alias
;;            #:room-alias
;;            #:room-id

;;            #:alias%resolve-room-alias

;;            #:alias%delete-room-alias

;;            #:alias%list-a-rooms-aliases

;;            #:rooms%my-joined-rooms

;;            #:rooms%invite-user-to-room
;;            #:user-id

;;            #:rooms%join-a-room
;;            #:third-party-signed

;;            #:rooms%join-a-room/alias-or-id
;;            #:room-id-or-alias
;;            #:server-name
;;            #:third-party-signed

;;            #:rooms%leave-a-room

;;            #:rooms%kick-user-from-room
;;            #:reason

;;            #:rooms%ban-user-from-room

;;            #:rooms%unban-user-from-room

;;            #:rooms%set-room-visibility

;;            #:rooms%public-rooms
;;            #:limit
;;            #:since
;;            #:server

;;            #:rooms%public-rooms/filtered
;;            #:filter
;;            #:include-all-networks
;;            #:third-party-instance-id))

(in-package #:lunamech-matrix-api/v2/api)

(defapi%post create-room ("createRoom")
             "Create a new room with various configuration options."
             ((visibility
               :accessor visibility
               :initarg :visibility
               :initform "private"
               :one-of '("public" "private"))
              (room-alias-name
               :accessor room-alias-name
               :initarg :room-alias-name)
              (name
               :accessor name
               :initarg :name)
              (topic
               :accessor topic
               :initarg :topic)
              (invite
               :accessor invite
               :initarg :invite)
              (invite-3pid
               :accessor invite-3pid
               :initarg :invite-3pid)
              (room-version
               :accessor room-version
               :initarg :room-version)
              (creation-content
               :accessor creation-content
               :initarg :creation-content)
              (initial-state
               :accessor initial-state
               :initarg :initial-state)
              (preset
               :accessor preset
               :initarg :preset
               :one-of '("private_chat" "public_chat" "trusted_private_chat"))
              (is-direct
               :accessor is-direct
               :initarg :is-direct)
              (power-level-content-override
               :accessor power-level-content-override
               :initarg :power-level-content-override))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%put alias%new-room-alias ("directory/room/:room-alias")
            "Create a new mapping from room alias to room ID."
            ((room-alias
              :accessor room-alias
              :initarg :room-alias
              :in-url-p t
              :requiredp t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get alias%resolve-room-alias ("directory/room/:room-alias")
            "Requests that the server resolve a room alias to a room ID."
            ((room-alias
              :accessor room-alias
              :initarg :room-alias
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%delete alias%delete-room-alias ("directory/room/:room-alias")
               "Remove a mapping of alias to room ID"
               ((room-alias
                 :accessor room-alias
                 :initarg :room-alias
                 :in-url-p t
                 :requiredp t))
               (:rate-limited-p nil)
               (:requires-auth-p t))

(defapi%get alias%list-a-rooms-aliases ("directory/rooms/:room-id/aliases")
            "Get a list of aliases maintained by the local server for the given room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get rooms%my-joined-rooms ("joined_rooms")
            "Get a list of the users joined rooms."
            ()
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post rooms%invite-user-to-room ("rooms/:room-id/invite")
             "This API invites a user to participate in a particular room. They do not start participating in the room until they actually join the room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%join-a-room ("rooms/:room-id/join")
             "This API invites a user to participate in a particular room. They do not start participating in the room until they actually join the room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (third-party-signed
               :accessor third-party-signed
               :initarg :third-party-signed
               :requiredp nil))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%join-a-room/alias-or-id ("join/:room-id-or-alias")
             "This API invites a user to participate in a particular room. They do not start participating in the room until they actually join the room."
             ((room-id-or-alias
               :accessor room-id-or-alias
               :initarg :room-id-or-alias
               :in-url-p t
               :requiredp t)
              (server-name
               :accessor server-name
               :initarg :server-name
               :query-param-p t)
              (third-party-signed
               :accessor third-party-signed
               :initarg :third-party-signed
               :requiredp nil))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%leave-a-room ("rooms/:room-id/leave")
             "This API starts a user participating in a particular room, if that user is allowed to participate in that room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%forget-a-room ("rooms/:room-id/forget")
             "This API stops a user remembering about a particular room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%kick-user-from-room ("rooms/:room-id/kick")
             "Kick a user from the room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t)
              (reason
               :accessor reason
               :initarg :reason
               :initform "Shenanigans!"
               :requiredp t))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post rooms%ban-user-from-room ("rooms/:room-id/ban")
             "Ban a user from the room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t)
              (reason
               :accessor reason
               :initarg :reason
               :initform "Shenanigans!"
               :requiredp t))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post rooms%unban-user-from-room ("rooms/:room-id/unban")
             "Unban a user from the room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t))
             (:rate-limited-p nil)
             (:requires-auth-p t))


(defapi%get rooms%room-visibility ("directory/list/room/:room-id")
            "Gets the visibility of a given room on the server's public room directory."
            ((room-id 
              :accessor room-id 
              :initarg :room-id 
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%put rooms%set-room-visibility ("directory/list/room/:room-id")
            "Sets the visibility of a given room in the server's public room directory."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t)
             (visibility 
              :accessor visibility
              :initarg :visibility
              :initform "public"
              :one-of '("private" "public")
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get rooms%public-rooms ("publicRooms")
            "List the public rooms on the server."
            ((limit 
              :accessor limit
              :initarg :limit
              :query-param-p t)
             (since
              :accessor since
              :initarg :since
              :query-param-p t)
             (server
              :accessor server
              :initarg :server
              :query-param-p t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%post rooms%public-rooms/filtered ("publicRooms")
             "List the public rooms on the server with optional filter"
             ((limit 
               :accessor limit
               :initarg :limit)
              (since
               :accessor since
               :initarg :since)
              (filter
               :accessor filter
               :initarg :filter)
              (include-all-networks
               :accessor include-all-networks
               :initarg :include-all-networks)
              (third-party-instance-id
               :accessor third-party-instance-id
               :initarg :third-party-instance-id)
              (server
               :accessor server
               :initarg :server
               :query-param-p t))
             (:rate-limited-p nil)
             (:requires-auth-p t))












