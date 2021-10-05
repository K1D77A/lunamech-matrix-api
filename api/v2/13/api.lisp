(defpackage #:lunamech-matrix-api/v2/13
  (:use #:lunamech-matrix-api/v2 #:cl))

(in-package #:lunamech-matrix-api/v2/13)

(defapi%get get-turnserver-credentials ("voip/turnServer")
            "This API provides credentials for the client to use when initiating calls."
            ((username
              :accessor username
              :initarg :username
              :requiredp t)
             (password
              :accessor password
              :initarg :password
              :requiredp t)
             (urls
              :accessor urls
              :initarg :urls
              :requiredp t)
             (ttl
              :accessor ttl
              :initarg :ttl
              :requiredp t))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%put rooms%put-typing-notification ("rooms/:room-id/typing/:user-id")
            "This tells the server that the user is typing for the next N milliseconds where N is the value specified in the timeout key. Alternatively, if typing is false, it tells the server that the user has stopped typing."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (room-id 
              :accessor room-id
              :initarg :room-id 
              :in-url-p t
              :requiredp t)
             (typing
              :accessor typing
              :initarg :typing
              :initform "true"
              :requiredp t)
             (timeout
              :accessor timeout
              :initarg :timeout
              :initform 1000))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%post rooms%send-event-receipt ("rooms/:room-id/receipt/:receipt-type/:event-id")
             "This API updates the marker for the given receipt type to the event ID specified."
             ((room-id 
               :accessor room-id
               :initarg :room-id 
               :in-url-p t
               :requiredp t)
              (receipt-type
               :accessor receipt-type
               :initarg :receipt-type
               :in-url-p t
               :requiredp t)
              (event-id
               :accessor event-id
               :initarg :event-id
               :in-url-p t
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%set-read-receipt ("rooms/:room-id/read_markers")
             "Sets the position of the read marker for a given room, and optionally the read receipt's location."
             ((room-id 
               :accessor room-id
               :initarg :room-id 
               :in-url-p t
               :requiredp t)
              (m-fully-read
               :accessor m-fully-read
               :initarg :m-fully-read
               :name->json "m.fully_read"
               :requiredp t)
              (m-read
               :accessor m-read
               :initarg :m-read
               :name->json "m.read"))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%put presence%set-presence ("presence/:user-id/status")
            "This API sets the given user's presence state. When setting the status, the activity time is updated to reflect that activity; the client does not need to specify the last_active_ago field. You cannot set the presence state of another user."
            ((user-id 
              :accessor user-id
              :initarg :user-id 
              :in-url-p t
              :requiredp t)
             (presence
              :accessor presence
              :initarg :presence
              :one-of '("online" "offline" "unavailable")
              :requiredp t)
             (status-msg
              :accessor status-msg
              :initarg :status-msg))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get presence%get-presence ("presence/:user-id/status")
            "Get the given user's presence state"
            ((user-id 
              :accessor user-id
              :initarg :user-id 
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post media%upload ("upload")
             "Upload some content to the content repository"
             ((content-type
               :accessor content-type
               :initarg :content-type
               :category nil)
              (filename
               :accessor filename
               :initarg :filename
               :requiredp t
               :query-param-p t)
              (bytes
               :accessor bytes
               :initarg :bytes
               :requiredp t
               :specialp t))
             (:content-type content-type)
             (:specialp t)
             (:api "/_matrix/media/r0/")
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%get media%get-media ("download/:server-name/:media-id")
            "Download content from content repo"
            ((server-name
              :accessor server-name
              :initarg :server-name
              :in-url-p t
              :requiredp t)
             (media-id
              :accessor media-id
              :initarg :media-id
              :in-url-p t
              :requiredp t)
             (allow-remote
              :accessor allow-remote
              :initarg :allow-remote
              :query-param-p t
              :initform "true"))
            (:api "/_matrix/media/r0/")
            (:rate-limited-p t)
            (:requires-auth-p nil))

(defapi%get media%get-media/filename ("download/:server-name/:media-id/:filename")
            "Download content from content repo, with a filename"
            ((server-name
              :accessor server-name
              :initarg :server-name
              :in-url-p t
              :requiredp t)
             (media-id
              :accessor media-id
              :initarg :media-id
              :in-url-p t
              :requiredp t)
             (filename
              :accessor filename
              :initarg :filename
              :in-url-p t
              :requiredp t)
             (allow-remote
              :accessor allow-remote
              :initarg :allow-remote
              :query-param-p t
              :initform "true"))
            (:api "/_matrix/media/r0/")
            (:rate-limited-p t)
            (:requires-auth-p nil))

(defapi%get media%get-thumbnail ("thumbnail/:server-name/:media-id")
            "Download a thumbnail for content from content repo"
            ((server-name
              :accessor server-name
              :initarg :server-name
              :in-url-p t
              :requiredp t)
             (media-id
              :accessor media-id
              :initarg :media-id
              :in-url-p t
              :requiredp t)
             (width
              :accessor width
              :initarg :width
              :query-param-p t
              :requiredp t)
             (height
              :accessor height
              :initarg :height
              :query-param-p t
              :requiredp t)
             (resize-method
              :accessor resize-method
              :initarg :resize-method
              :name->json "method"
              :query-param-p t
              :requiredp nil
              :one-of '("crop" "scale"))
             (allow-remote
              :accessor allow-remote
              :initarg :allow-remote
              :query-param-p t
              :requiredp nil))
            (:api "/_matrix/media/r0/")
            (:rate-limited-p t)
            (:requires-auth-p nil))

(defapi%get media%get-preview ("preview_url")
            "Get information about a URL for the client. Typically this is called when a client sees a URL in a message and wants to render a preview for the user."
            ((url
              :accessor url
              :initarg :url
              :query-param-p t
              :requiredp t)
             (ts
              :accessor ts
              :initarg :ts
              :query-param-p t
              :requiredp nil))
            (:api "/_matrix/media/r0/")
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get media%get-config ("config")
            "This endpoint allows clients to retrieve the configuration of the content repository, such as upload limitations. Clients SHOULD use this as a guide when using content repository endpoints. All values are intentionally left optional. Clients SHOULD follow the advice given in the field description when the field is not available."
            ()
            (:api "/_matrix/media/r0/")
            (:rate-limited-p t)
            (:requires-auth-p t))












