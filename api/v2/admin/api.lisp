(in-package #:lunamech-matrix-api/v2/api)

(defapi%post admin%renew-account ("account_validity/validity")
             "This API extends the validity of an account by as much time as configured in the period parameter from the account_validity configuration."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t)
              (expiration-ts
               :accessor expiration-ts
               :initarg :expiration-ts
               :requiredp t)
              (enable-renewal-emails
               :accessor enable-renewal-emails
               :initarg :enable-renewal-emails
               :initform "false"
               :one-of ("true" "false")
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%delete-group ("delete_group/:group-id")
             "This API lets a server admin delete a local group. Doing so will kick all users out of the group so that their clients will correctly handle the group being deleted."
             ((group-id
               :accessor group-id
               :initarg :group-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-event-reports ("event_reports")
            "This API returns information about reported events."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :query-param-p t
              :requiredp nil)
             (limit
              :accessor limit
              :initarg :limit
              :initform 100 
              :query-param-p t
              :requiredp nil)
             (from
              :accessor from
              :initarg :from 
              :initform 0
              :query-param-p t
              :requiredp nil)
             (dir
              :accessor dir
              :initarg :dir 
              :initform "b"
              :query-param-p t
              :one-of ("b" "f")
              :requiredp nil)
             (room-id
              :accessor room-id
              :initarg :room-id
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-specific-event-report ("event_reports/:report-id")
            "This API returns information about a specific event report."
            ((report-id
              :accessor report-id
              :initarg :report-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-a-rooms-media ("room/:room-id/media")
            "This API gets a list of known media in a room. However, it only shows media from unencrypted events or rooms."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%quarantine-media-by-id ("media/quarantine/:server-name/:media-id")
             "This API quarantines a single piece of local or remote media."
             ((media-id
               :accessor media-id
               :initarg :media-id
               :in-url-p t
               :requiredp t)
              (server-name
               :accessor server-name
               :initarg :server-name
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%unquarantine-media-by-id ("media/unquarantine/:server-name/:media-id")
             "This API unquarantines a single piece of local or remote media."
             ((media-id
               :accessor media-id
               :initarg :media-id
               :in-url-p t
               :requiredp t)
              (server-name
               :accessor server-name
               :initarg :server-name
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%quarantine-media-in-room ("room/:room-id/media/quarantine")
             "This API quarantines all local and remote media in a room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%quarantine-users-media ("user/:user-id/media/quarantine")
             "This API quarantines all local media that a local user has uploaded. That is to say, if you would like to quarantine media uploaded by a user on a remote homeserver, you should instead use one of the other APIs."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%protect-media-by-id ("media/protect/:media-id")
             "This API protects a single piece of local media from being quarantined using the above APIs. This is useful for sticker packs and other shared media which you do not want to get quarantined, especially when quarantining media in a room."
             ((media-id
               :accessor media-id
               :initarg :media-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%unprotect-media-by-id ("media/unprotect/:media-id")
             "This API reverts the protection of a media."
             ((media-id
               :accessor media-id
               :initarg :media-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%delete admin%delete-media-by-id ("media/:server-name/:media-id")
               "Delete a specific media_id."
               ((media-id
                 :accessor media-id
                 :initarg :media-id
                 :in-url-p t
                 :requiredp t)
                (server-name
                 :accessor server-name
                 :initarg :server-name
                 :in-url-p t
                 :requiredp t))
               (:api "/_synapse/admin/v1/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%post admin%delete-media-by-date-or-size ("media/:server-name/delete")
             "Delete media before TS."
             ((before-ts 
               :accessor before-ts 
               :initarg :before-ts 
               :query-param-p t
               :requiredp t)
              (server-name
               :accessor server-name
               :initarg :server-name
               :in-url-p t
               :requiredp t)
              (size-gt
               :accessor size-gt
               :initarg :size-gt
               :initform 0
               :query-param-p t
               :requiredp nil)
              (keep-profiles 
               :accessor keep-profiles
               :initarg :keep-profiles 
               :initform "true"
               :one-of ("true" "false")
               :query-param-p t
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%purge-remote-media-cache ("purge_media_cache")
             "Delete media before TS."
             ((unix-timestamp-in-ms
               :accessor unix-timestamp-in-ms
               :initarg :unix-timestamp-in-ms
               :query-param-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%purge-room-history ("purge_history/:room-id")
             "The purge history API allows server admins to purge historic events from their database, reclaiming disk space.

Depending on the amount of history being purged a call to the API may take several minutes or longer. During this period users will not be able to paginate further back in the room from the point being purged from.

Note that Synapse requires at least one message in each room, so it will never delete the last message in a room."
             ((room-id
               :accessor room-id
               :initarg :room-id
               :in-url-p t
               :requiredp t)
              (delete-local-events
               :accessor delete-local-events
               :initarg :delete-local-events
               :initform "false"
               :one-of ("true" "false")
               :requiredp nil)
              (purge-up-to-event-id
               :accessor purge-up-to-event-id
               :initarg :purge-up-to-event-id
               :requiredp nil)
              (purge-up-to-ts
               :accessor purge-up-to-ts
               :initarg :purge-up-to-ts
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-purge-status ("purge_history_status/:purge-id")
            "It is possible to poll for updates on recent purges with a second API;"
            ((purge-id
              :accessor purge-id
              :initarg :purge-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-registration-nonce ("register")
            "To fetch the nonce, you need to request one from the API"
            ()
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%register-user ("register")
             "Once you have the nonce, you can make a POST to the same URL with a JSON body containing the nonce, username, password, whether they are an admin (optional, False by default), and a HMAC digest of the content. Also you can set the displayname (optional, username by default)."
             ((nonce
               :accessor nonce
               :initarg :nonce
               :requiredp t)
              (username
               :accessor username
               :initarg :username
               :requiredp nil)
              (display-name
               :accessor display-name
               :initarg :display-name
               :requiredp nil)
              (admin
               :accessor admin
               :initarg :admin
               :initform "false"
               :requiredp t
               :one-of ("true" "false"))
              (mac
               :accessor mac
               :initarg :mac
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-all-tokens ("registration_tokens")
            "Lists all tokens and details about them. If the request is successful, the top level JSON object will have a registration_tokens key which is an array of registration token objects."
            ((valid
              :accessor valid
              :initarg :valid
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-one-token ("registration_tokens/:token")
            "Get details about a single token. If the request is successful, the response body will be a registration token object."
            ((token
              :accessor token
              :initarg :token 
              :in-url-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%create-token ("registration_tokens/new")
             "Create a new registration token. If the request is successful, the newly created token will be returned as a registration token object in the response body."
             ((token
               :accessor token
               :initarg :token
               :requiredp nil)
              (uses-allowed
               :accessor uses-allowed
               :initarg :uses-allowed
               :requiredp nil)
              (expiry-time
               :accessor expiry-time 
               :initarg :expiry-time 
               :requiredp nil)
              (token-length 
               :accessor token-length
               :initarg :token-length
               :name->json "length"
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%put admin%update-token ("registration_tokens/:token")
            "Update the number of allowed uses or expiry time of a token. If the request is successful, the updated token will be returned as a registration token object in the response body."
            ((token
              :accessor token
              :initarg :token
              :in-url-p t
              :requiredp t)
             (uses-allowed
              :accessor uses-allowed
              :initarg :uses-allowed
              :requiredp nil)
             (expiry-time
              :accessor expiry-time 
              :initarg :expiry-time 
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%delete admin%delete-token ("registration_tokens/:token")
               "Delete a registration token. If the request is successful, the response body will be an empty JSON object."
               ((token
                 :accessor token
                 :initarg :token
                 :in-url-p t
                 :requiredp t))
               (:api "/_synapse/admin/v1/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%post admin%edit-users-room-membershp ("join/:room-id-or-alias")
             "This API allows an administrator to join an user account with a given user_id to a room with a given room_id_or_alias. You can only modify the membership of local users. The server administrator must be in the room and have permission to invite users."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t)
              (room-id-or-alias
               :accessor room-id-or-alias
               :initarg :room-id-or-alias
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))




