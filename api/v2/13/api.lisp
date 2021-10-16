(in-package #:lunamech-matrix-api/v2)

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
              :initform t
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
              :one-of ("online" "offline" "unavailable")
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
            (:do-not-decode-p t)
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
            (:do-not-decode-p t)            
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
              :one-of ("crop" "scale"))
             (allow-remote
              :accessor allow-remote
              :initarg :allow-remote
              :query-param-p t
              :requiredp nil))
            (:api "/_matrix/media/r0/")
            (:do-not-decode-p t)
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


(defapi%put protocol%send-to-device ("sendToDevice/:event-type/:txn")
            "This endpoint is used to send send-to-device events to a set of client devices."
            ((event-type
              :accessor event-type
              :initarg :event-type
              :in-url-p t
              :requiredp t)
             (txn
              :accessor txn
              :initarg :txn
              :in-url-p t
              :requiredp t)
             (messages
              :accessor messages
              :initarg :messages
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%get devices%get-devices ("devices")
            "Gets information about all devices for the current user."
            ()
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get devices%get-device ("devices/:device-id")
            "Gets information about a single device for the current user."
            ((device-id
              :accessor device-id
              :initarg :device-id
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))


(defapi%put devices%update-device ("devices/:device-id")
            "Updates the metadata on the given device."
            ((device-id
              :accessor device-id
              :initarg :device-id
              :in-url-p t
              :requiredp t)
             (display-name
              :accessor display-name
              :initarg :display-name))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%delete devices%delete-device ("devices/:device-id")
               "Deletes the given device, and invalidates any access token associated with it."
               ((device-id
                 :accessor device-id
                 :initarg :device-id
                 :in-url-p t
                 :requiredp t)
                (auth
                 :accessor auth
                 :initarg :auth))
               (:rate-limited-p nil)
               (:requires-auth-p t))

(defapi%post devices%delete-devices ("delete_devices")
             "Deletes the given devices, and invalidates any access token associated with them."
             ((devices 
               :accessor devices 
               :initarg :devices 
               :requiredp t)
              (auth
               :accessor auth
               :initarg :auth))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post keys%upload-keys ("keys/upload")
             "Publishes end-to-end encryption keys for the device."
             ((device-keys 
               :accessor device-keys
               :initarg :device-keys
               :requiredp t)
              (one-time-keys
               :accessor one-time-keys
               :initarg :one-time-keys))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post keys%download-devices-and-keys ("keys/query")
             "Returns the current devices and identity keys for the given users."
             ((timeout
               :accessor timeout
               :initarg :timeout 
               :initform 10000)
              (device-keys 
               :accessor device-keys
               :initarg :device-keys
               :requiredp t)
              (token
               :accessor token
               :initarg :token
               :requiredp nil))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post keys%claim-keys ("keys/claim")
             "Claims one-time keys for use in pre-key messages."
             ((timeout
               :accessor timeout
               :initarg :timeout
               :initform 10000)
              (one-time-keys
               :accessor one-time-keys
               :initarg :one-time-keys
               :requiredp t))
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%get keys%get-key-changes ("keys/changes")
            "Gets a list of users who have updated their device identity keys since a previous sync token."
            ((from
              :accessor from
              :initarg :from
              :requiredp t
              :query-param-p t)
             (to
              :accessor to
              :initarg :to
              :requiredp t
              :query-param-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get pushers%get-active-pushers ("pushers")
            "Gets all currently active pushers for the authenticated user."
            ((pushers
              :accessor pushers
              :initarg :pushers))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post pushers%set-pusher ("pushers/set")
             "Claims one-time keys for use in pre-key messages."
             ((pushkey 
               :accessor pushkey
               :initarg :pushkey
               :requiredp t)
              (kind 
               :accessor kind
               :initarg :kind
               :one-of ("http" "email" "null")
               :requiredp t)
              (app-id 
               :accessor app-id 
               :initarg :app-id
               :requiredp t)
              (app-display-name
               :accessor app-display-name 
               :initarg :app-display-name
               :requiredp t)
              (device-display-name
               :accessor device-display-name 
               :initarg :device-display-name
               :requiredp t)
              (profile-tag
               :accessor profile-tag 
               :initarg :profile-tag
               :requiredp t)
              (lang
               :accessor lang
               :initarg :lang 
               :requiredp t)
              (data
               :accessor data 
               :initarg :data 
               :requiredp t)
              (append-bool
               :accessor append-bool
               :initarg :append-bool
               :name->json "append"
               :initform nil))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%get notifications%get-notifications ("notifications")
            "This API is used to paginate through the list of events that the user has been, or would have been notified about."
            ((from
              :accessor from
              :initarg :from
              :query-param-p t)
             (only
              :accessor only
              :initarg :only
              :initform "highlight"
              :query-param-p t)
             (limit 
              :accessor limit
              :initarg :limit
              :query-param-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get pushrules%get-pushrules ("pushrules/:drill-down")
            "Retrieve all push rulesets for this user. Clients can \"drill-down\" on the rulesets by suffixing a scope to this path e.g. /pushrules/global/. This will return a subset of this data under the specified key e.g. the global key."
            ((global
              :accessor global
              :initarg :global
              :requiredp t)
             (drill-down
              :accessor drill-down
              :initarg :drill-down
              :in-url-p t
              :requiredp nil))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get pushrules%get-specific-pushrule ("pushrules/:scope/:kind/:rule-id")
            "Retrieve a single specified push rule."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :requiredp t
              :one-of ("override" "underride" "sender" "room" "content")
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%delete pushrules%delete-specific-pushrule ("pushrules/:scope/:kind/:rule-id")
               "Delete a single specified push rule."
               ((scope
                 :accessor scope 
                 :initarg :scope 
                 :requiredp t
                 :in-url-p t)
                (kind 
                 :accessor kind 
                 :initarg :kind
                 :one-of ("override" "underride" "sender" "room" "content")
                 :requiredp t
                 :in-url-p t)
                (rule-id 
                 :accessor rule-id 
                 :initarg :rule-id 
                 :requiredp t
                 :in-url-p t))
               (:rate-limited-p nil)
               (:requires-auth-p t))

(defapi%put pushrules%create-pushrule ("pushrules/:scope/:kind/:rule-id")
            "This endpoint allows the creation, modification and deletion of pushers for this user ID. The behaviour of this endpoint varies depending on the values in the JSON body."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :one-of ("override" "underride" "sender" "room" "content")
              :requiredp t
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t)
             (before
              :accessor before
              :initarg :before
              :query-param-p t)
             (after
              :accessor after
              :initarg :after
              :query-param-p t)
             (actions
              :accessor actions
              :initarg :actions
              :one-of ("notify" "dont_notify" "coalesce" "set_tweak")
              :requiredp t)
             (conditions
              :accessor conditions
              :initarg :conditions)
             (pattern
              :accessor pattern
              :initarg :pattern))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get pushrules%pushrule-enabled ("pushrules/:scope/:kind/:rule-id/enabled")
            "This endpoint gets whether the specified push rule is enabled."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :initform "global"
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :one-of ("override" "underride" "sender" "room" "content")
              :requiredp t
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%put pushrules%enable-pushrule ("pushrules/:scope/:kind/:rule-id/enabled")
            "This endpoint allows clients to enable or disable the specified push rule."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :initform "global"
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :one-of ("override" "underride" "sender" "room" "content")
              :requiredp t
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t)
             (enabled
              :accessor enabled
              :initarg :enabled
              :requiredp t
              :initform "true"))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get pushrules%pushrule-actions ("pushrules/:scope/:kind/:rule-id/actions")
            "This endpoint get the actions for the specified push rule."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :initform "global"
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :one-of ("override" "underride" "sender" "room" "content")
              :requiredp t
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%put pushrules%change-a-pushrule-actions ("pushrules/:scope/:kind/:rule-id/actions")
            "This endpoint allows clients to change the actions of a push rule. This can be used to change the actions of builtin rules."
            ((scope
              :accessor scope 
              :initarg :scope 
              :requiredp t
              :initform "global"
              :in-url-p t)
             (kind 
              :accessor kind 
              :initarg :kind
              :one-of ("override" "underride" "sender" "room" "content")
              :requiredp t
              :in-url-p t)
             (rule-id 
              :accessor rule-id 
              :initarg :rule-id 
              :requiredp t
              :in-url-p t)
             (actions
              :accessor actions
              :initarg :actions
              :one-of ("notify" "dont_notify" "coalesce" "set_tweak")              
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post rooms%invite-user-to-room/3pid ("rooms/:room-id/invite")
             "This API invites a user to participate in a particular room. They do not start participating in the room until they actually join the room. Uses 3pid."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (id-server
               :accessor id-server
               :initarg :id-server
               :requiredp t)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token
               :requiredp t)
              (medium 
               :accessor medium 
               :initarg :medium
               :requiredp t)
              (address 
               :accessor address 
               :initarg :address 
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post server-side-search ("search")
             "Performs a full text search across different categories"
             ((next-batch
               :accessor next-batch
               :initarg :next-batch
               :query-param-p t)
              (search-categories
               :accessor search-categories
               :initarg :search-categories
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%get wait-for-events ("events")
            "This will listen for new events related to a particular room and return them to the caller. This will block until an event is received, or until the timeout is reached."
            ((from
              :accessor from
              :initarg :from
              :query-param-p t)
             (timeout
              :accessor timeout
              :initarg :timeout
              :initform 10000
              :query-param-p t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :query-param-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get tags%list-tags ("user/:user-id/rooms/:room-id/tags")
            "List the tags set by a user on a room."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%put tags%set-tag ("user/:user-id/rooms/:room-id/tags/:tag")
            "Add a tag to the room."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (tag
              :accessor tag
              :initarg :tag
              :requiredp t
              :in-url-p t)
             (order
              :accessor order
              :initarg :order
              :initform 0))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%delete tags%delete-tag ("user/:user-id/rooms/:room-id/tags/:tag")
               "Add a tag to the room."
               ((user-id
                 :accessor user-id
                 :initarg :user-id
                 :requiredp t
                 :in-url-p t)
                (room-id
                 :accessor room-id
                 :initarg :room-id
                 :requiredp t
                 :in-url-p t)
                (tag
                 :accessor tag
                 :initarg :tag
                 :requiredp t
                 :in-url-p t))
               (:rate-limited-p nil)
               (:requires-auth-p t))

(defapi%put account-data%set-data ("user/:user-id/account_data/:data-type")
            "Set some account_data for the client. This config is only visible to the user that set the account_data. The config will be synced to clients in the top-level account_data."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t)
             (data-type 
              :accessor data-type
              :initarg :data-type
              :name->json "type"
              :requiredp t
              :in-url-p t)
             (body
              :accessor body
              :initarg :body
              :requiredp t
              :specialp t))
            (:specialp t)
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get account-data%get-data ("user/:user-id/account_data/:data-type")
            "Get some account_data for the client. This config is only visible to the user that set the account_data."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t)
             (data-type 
              :accessor data-type
              :initarg :data-type
              :name->json "type"
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%put account-data%set-data-in-room
    ("user/:user-id/rooms/:room-id/account_data/:data-type")
    "Set some account_data for the client on a given room. This config is only visible to the user that set the account_data. The config will be synced to clients in the per-room account_data."
    ((user-id
      :accessor user-id
      :initarg :user-id
      :requiredp t
      :in-url-p t)
     (room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (data-type 
      :accessor data-type
      :initarg :data-type
      :name->json "type"
      :requiredp t
      :in-url-p t))
    (:rate-limited-p nil)
    (:requires-auth-p t))

(defapi%get account-data%get-data-in-room
    ("user/:user-id/rooms/:room-id/account_data/:data-type")
    "Get some account_data for the client on a given room. This config is only visible to the user that set the account_data."
    ((user-id
      :accessor user-id
      :initarg :user-id
      :requiredp t
      :in-url-p t)
     (room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (data-type 
      :accessor data-type
      :initarg :data-type
      :name->json "type"
      :requiredp t
      :in-url-p t))
    (:rate-limited-p nil)
    (:requires-auth-p t))

(defapi%get admin%whois-user ("admin/whois/:user-id")
            "Gets information about a particular user."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get rooms%events-before-and-after ("rooms/:room-id/context/:event-id")
            "This API returns a number of events that happened just before and after the specified event. This allows clients to get the context surrounding an event."
            ((event-id
              :accessor event-id
              :initarg :event-id
              :requiredp t
              :in-url-p t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (limit
              :accessor limit
              :initarg :limit
              :query-param-p t)
             (filter
              :accessor filter
              :initarg :filter
              :query-param-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get sso%sso-url ("login/sso/redirect")
            "A web-based Matrix client should instruct the user's browser to navigate to this endpoint in order to log in via SSO."
            ((redirect-url
              :accessor redirect-url
              :initarg :redirect-url
              :query-param-p t
              :requiredp t))            
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%get rooms%report-content ("rooms/:room-id/report/:event-id")
            "Reports an event as inappropriate to the server, which may then notify the appropriate people"
            ((event-id
              :accessor event-id
              :initarg :event-id
              :requiredp t
              :in-url-p t)
             (room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (score
              :accessor score
              :initarg :score
              :initform -50
              :requiredp t)
             (reason
              :accessor reason
              :initarg :reason
              :initform "This makes me sad."
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%get-protocols-metadata ("thirdparty/protocols")
            "Fetches the overall metadata about protocols supported by the homeserver. Includes both the available protocols and all fields required for queries against each protocol."
            ()
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%get-protocol-metadata ("thirdparty/protocol/:protocol")
            "Fetches the metadata from the homeserver about a particular third party protocol."
            ((protocol
              :accessor protocol
              :initarg :protocol
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%get-protocol-location ("thirdparty/location/:protocol")
            "Requesting this endpoint with a valid protocol name results in a list of successful mapping results in a JSON array. Each result contains objects to represent the Matrix room or rooms that represent a portal to this third party network. Each has the Matrix room alias string, an identifier for the particular third party network protocol, and an object containing the network-specific fields that comprise this identifier. It should attempt to canonicalise the identifier as much as reasonably possible given the network type."
            ((protocol
              :accessor protocol
              :initarg :protocol
              :requiredp t
              :in-url-p t)
             (search-fields
              :accessor search-fields
              :initarg :search-fields
              :name->json "searchFields"
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%get-protocol-users ("thirdparty/user/:protocol")
            "Retrieve a Matrix User ID linked to a user on the third party service, given a set of user parameters."
            ((protocol
              :accessor protocol
              :initarg :protocol
              :requiredp t
              :in-url-p t)
             (fields
              :accessor fields
              :initarg :fields
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%get-thirdparty-locations ("thirdparty/location")
            "Retrieve an array of third party network locations from a Matrix room alias."
            ((alias 
              :accessor alias 
              :initarg :alias 
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get thirdparty%thirdparty-for-user ("thirdparty/user")
            "Retrieve an array of third party users from a Matrix User ID."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :query-param-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post openid%request-openid ("user/:user-id/openid/request_token")
             "Gets an OpenID token object that the requester may supply to another service to verify their identity in Matrix. The generated token is only valid for exchanging for user information from the federation API for OpenID."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post rooms%upgrade-room ("rooms/:room-id/upgrade")
             "Upgrades the given room to a particular room version."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (new-version
               :accessor new-version
               :initarg :new-version
               :in-url-p t
               :requiredp t))
             (:rate-limited-p nil)
             (:requires-auth-p t))

