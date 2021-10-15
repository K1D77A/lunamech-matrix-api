(in-package #:lunamech-matrix-api/v2)

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

(defapi%get admin%list-rooms ("rooms")
            "The List Room admin API allows server admins to get a list of rooms on their server. There are various parameters available that allow for filtering and sorting the returned list. This API supports pagination."
            ((limit
              :accessor limit
              :initarg :limit
              :initform 100 
              :query-param-p t
              :requiredp nil)
             (order-by
              :accessor order-by
              :initarg :order-by
              :initform "name"
              :one-of ("name" "canonical_alias"
                              "joined_members" "joined_local_members"
                              "version" "creator" "encryption" "federatable"
                              "public" "join_rules" "guest_access" "history_visibility"
                              "state_events")
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
              :initform "f"
              :query-param-p t
              :one-of ("b" "f")
              :requiredp nil)
             (search-term
              :accessor search-term
              :initarg :search-term
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-room-details ("rooms/:room-id")
            "The Room Details admin API allows server admins to get all details of a room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))


(defapi%get admin%get-room-members ("rooms/:room-id/members")
            "The Room Members admin API allows server admins to get a list of all members of a room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-room-state ("rooms/:room-id/state")
            "The Room State admin API allows server admins to get a list of all state events in a room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%delete admin%delete-room ("rooms/:room-id")
               "The Delete Room admin API allows server admins to remove rooms from server and block these rooms."
               ((room-id
                 :accessor room-id
                 :initarg :room-id
                 :in-url-p t
                 :requiredp t)
                (new-room-user-id
                 :accessor new-room-user-id
                 :initarg :new-room-user-id
                 :requiredp nil)
                (room-name
                 :accessor room-name
                 :initarg :room-name
                 :requiredp nil)
                (message
                 :accessor message 
                 :initarg :message
                 :initform "Room shutdown by admin."
                 :requiredp nil)
                (block-room
                 :accessor block-room
                 :initarg block-room
                 :name->json "block"
                 :initform "false"
                 :requiredp nil)
                (purge
                 :accessor purge
                 :initarg :purge
                 :initform "true"
                 :requiredp nil)
                (force-purge
                 :accessor force-purge
                 :initarg :force-purge
                 :initform "false"
                 :requiredp nil))
               (:api "/_synapse/admin/v1/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%post admin%make-user-admin-in-room ("join/:room-id-or-alias/make_room_admin")
             "Grants another user the highest power available to a local user who is in the room. If the user is not in the room, and it is not publicly joinable, then invite the user."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :requiredp nil)
              (room-id-or-alias
               :accessor room-id-or-alias
               :initarg :room-id-or-alias
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-room-forward-extremities
    ("join/:room-id-or-alias/forward_extremities")
    "To check the status of forward extremities for a room:"            
    ((room-id-or-alias
      :accessor room-id-or-alias
      :initarg :room-id-or-alias
      :in-url-p t
      :requiredp t))
    (:api "/_synapse/admin/v1/")
    (:requires-auth-p t)
    (:rate-limited-p nil))

(defapi%delete admin%delete-room-forward-extremities
    ("join/:room-id-or-alias/forward_extremities")
    "WARNING: Please ensure you know what you're doing and have read the related issue #1760. Under no situations should this API be executed as an automated maintenance task!"
    ((room-id-or-alias
      :accessor room-id-or-alias
      :initarg :room-id-or-alias
      :in-url-p t
      :requiredp t))
    (:api "/_synapse/admin/v1/")
    (:requires-auth-p t)
    (:rate-limited-p nil))

(defapi%get admin%get-event-context ("join/:room-id/context/:event-id")
            "This API lets a client find the context of an event. This is designed primarily to investigate abuse reports."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t)
             (event-id
              :accessor event-id
              :initarg :event-id
              :in-url-p t
              :requiredp t)
             (limit
              :accessor limit
              :initarg :limit
              :query-param-p t)
             (filter
              :accessor filter
              :initarg :filter
              :query-param-p t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%post-server-notice ("send_server_notice")
             "Sends a server notice."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t)
              (content
               :accessor content
               :initarg :content
               :requiredp t
               :specialp t)
              (event-type
               :accessor event-type
               :initarg :event-type
               :name->json "type"
               :requiredp nil)
              (state-key
               :accessor state-key
               :initarg :state-key
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:specialp t)
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%put admin%put-server-notice ("send_server_notice/:txn")
            "Sends a server notice."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t)
             (txn
              :accessor txn
              :Initarg :txn
              :requiredp t)
             (content
              :accessor content
              :initarg :content
              :requiredp t
              :specialp t)
             (event-type
              :accessor event-type
              :initarg :event-type
              :name->json "type"
              :requiredp nil)
             (state-key
              :accessor state-key
              :initarg :state-key
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:specialp t)
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-users-media-statistics ("statistics/users/media")
            "Returns information about all local media usage of users. Gives the possibility to filter them by time and user."
            ((limit
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
              :initform "f"
              :query-param-p t
              :one-of ("b" "f")
              :requiredp nil)
             (order-by
              :accessor order-by
              :initarg :order-by
              :initform "user_id"
              :one-of ("user_id" "displayname" "media_length" "media_count")
              :query-param-p t
              :requiredp nil)
             (search-term
              :accessor search-term
              :initarg :search-term
              :query-param-p t
              :requiredp nil)
             (until-ts
              :accessor until-ts
              :initarg :until-ts
              :query-param-p t
              :requiredp nil)
             (from-ts
              :accessor from-ts
              :initarg :from-ts
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-server-version ("server_version")
            "Gets the server version"
            ()
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p nil)
            (:rate-limited-p nil))

(defapi%get admin%query-user-account ("users/:user-id")
            "This API returns information about a specific user account."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%put admin%modify-user-account ("users/:user-id")
            "This API allows an administrator to create or modify a user account with a specific user_id."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp nil)
             (password
              :accessor password
              :initarg :password
              :requiredp nil)
             (displayname
              :accessor displayname
              :initarg :displayname
              :requiredp nil)
             (threepids 
              :accessor threepids 
              :initarg :threepids
              :requiredp nil
              :documentation "Array of threepids")
             (external-ids 
              :accessor external-ids 
              :initarg :external-ids 
              :requiredp nil
              :documentation "Array of external ids")
             (avatar-url 
              :accessor avatar-url 
              :initarg :avatar-url 
              :requiredp nil)
             (admin
              :accessor admin
              :initarg :admin
              :initform "false"
              :requiredp nil)
             (deactivated
              :accessor deactivated
              :initarg :deactivated
              :initform "false"
              :requiredp nil))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%list-accounts ("users")
            "This API returns all local user accounts. By default, the response is ordered by ascending user ID."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :query-param-p t
              :requiredp t)
             (name 
              :accessor name 
              :initarg :name 
              :query-param-p t
              :requiredp nil)
             (guests 
              :accessor guests 
              :initarg :guests
              :initform "false"
              :query-param-p t
              :requiredp nil)
             (deactivated
              :accessor deactivated
              :initarg :deactivated
              :initform "false"
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
              :initform "f"
              :query-param-p t
              :one-of ("b" "f")
              :requiredp nil)
             (order-by
              :accessor order-by
              :initarg :order-by
              :initform "user_id"
              :one-of ("name" "is_guest" "admin" "user_type" "deactivated" "shadow_banned"
                              "displayname" "avatar_url" "creation_ts")
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%query-current-sessions ("whois/:user-id")
            "This API returns information about the active sessions for a specific user."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))


(defapi%post admin%deactivate-account ("deactivate/:user-id")
             "This API deactivates an account. It removes active access tokens, resets the password, and deletes third-party IDs (to prevent the user requesting a password reset)."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t)
              (erase
               :accessor erase
               :initarg :erase
               :initform "true"
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%post admin%reset-password ("reset_password/:user-id")
             "Changes the password of another user. This will automatically log the user out of all their devices."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t)
              (logout-devices
               :accessor logout-devices
               :initarg :logout-devices
               :initform "true"
               :requiredp t)
              (new-password 
               :accessor new-password 
               :initarg :new-password
               :initform "passwordpassword"
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-user-admin-status ("users/:user-id/admin")
            "Get whether a user is a server administrator or not"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%put admin%set-user-admin-status ("users/:user-id/admin")
            "Set whether a user is a server administrator or not"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (admin
              :accessor admin
              :initarg :admin
              :initform "true"
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-users-room-memberships ("users/:user-id/joined_rooms")
            "Gets a list of all room_id that a specific user_id is member"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%get admin%get-users-uploaded-media ("users/:user-id/media")
            "Gets a list of all local media that a specific user_id has created. By default, the response is ordered by descending creation date and ascending media ID. The newest media is on top. You can change the order with parameters order_by and dir."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
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
              :initform "f"
              :query-param-p t
              :one-of ("b" "f")
              :requiredp nil)
             (order-by
              :accessor order-by
              :initarg :order-by
              :initform "user_id"
              :one-of ("media_id" "upload_name" "created_ts" "last_access_ts" "media_length"
                                  "media_type" "quarantined_by" "safe_from_quarantine")
              :query-param-p t
              :requiredp nil))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%delete admin%delete-users-media ("users/:user-id/media")
               "This API deletes the local media from the disk of your own server that a specific user_id has created. This includes any local thumbnails."
               ((user-id
                 :accessor user-id
                 :initarg :user-id
                 :in-url-p t
                 :requiredp t)
                (limit
                 :accessor limit
                 :initarg :limit
                 :initform 100 
                 :query-param-p t
                 :requiredp nil)
                (dir
                 :accessor dir
                 :initarg :dir 
                 :initform "f"
                 :query-param-p t
                 :one-of ("b" "f")
                 :requiredp nil)
                (order-by
                 :accessor order-by
                 :initarg :order-by
                 :initform "user_id"
                 :query-param-p t
                 :requiredp nil))
               (:api "/_synapse/admin/v1/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%post admin%login-as-user ("users/:user-id/login")
             "Get an access token that can be used to authenticate as that user. Useful for when admins wish to do actions on behalf of a user."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t)
              (valid-until-ms
               :accessor valid-until-ms
               :initarg :valid-until-ms
               :query-param-p t
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-users-devices ("users/:user-id/devices")
            "Gets information about all devices for a specific user_id."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%delete-users-devices ("users/:user-id/delete_devices")
             "Deletes the given devices for a specific user_id, and invalidates any access token associated with them."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t)
              (devices
               :accessor devices 
               :initarg :devices 
               :requiredp t))
             (:api "/_synapse/admin/v2/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%show-user-device ("users/:user-id/devices/:device-id")
            "Gets information on a single device, by device_id for a specific user_id."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (device-id 
              :accessor device-id 
              :initarg :device-id
              :in-url-p t 
              :requiredp t))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%put admin%update-user-device ("users/:user-id/devices/:device-id")
            "Updates the metadata on the given device_id for a specific user_id."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (device-id 
              :accessor device-id 
              :initarg :device-id
              :in-url-p t 
              :requiredp t)
             (display-name
              :accessor display-name
              :initarg :display-name
              :requiredp t))
            (:api "/_synapse/admin/v2/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%delete admin%delete-user-device ("users/:user-id/devices/:device-id")
               "Deletes the given device_id for a specific user_id, and invalidates any access token associated with it."
               ((user-id
                 :accessor user-id
                 :initarg :user-id
                 :in-url-p t
                 :requiredp t)
                (device-id 
                 :accessor device-id 
                 :initarg :device-id
                 :in-url-p t 
                 :requiredp t))
               (:api "/_synapse/admin/v2/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%get admin%get-users-pushers ("users/:user-id/pushers")
            "Gets information about all pushers for a specific user_id."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))

(defapi%post admin%shadowban-user ("users/:user-id/shadow_ban")
             "Shadow-banning is a useful tool for moderating malicious or egregiously abusive users. A shadow-banned users receives successful responses to their client-server API requests, but the events are not propagated into rooms. This can be an effective tool as it (hopefully) takes longer for the user to realise they are being moderated before pivoting to another account."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get admin%get-users-ratelimit ("users/:user-id/override_ratelimit")
            "Get status of ratelimit"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:api "/_synapse/admin/v1/")
            (:requires-auth-p t)
            (:rate-limited-p nil))


(defapi%post admin%set-users-ratelimit ("users/:user-id/override_ratelimit")
             "Get status of ratelimit"
             ((user-id
               :accessor user-id
               :initarg :user-id
               :in-url-p t
               :requiredp t)
              (messages-per-second
               :accessor messages-per-second
               :initarg :messages-per-second
               :initform 0
               :requiredp nil)
              (burst-count
               :accessor burst-count
               :initarg :burst-count 
               :initform 0
               :requiredp nil))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%delete admin%delete-users-ratelimit ("users/:user-id/override_ratelimit")
               "Delete a users ratelimit"
               ((user-id
                 :accessor user-id
                 :initarg :user-id
                 :in-url-p t
                 :requiredp t))
               (:api "/_synapse/admin/v1/")
               (:requires-auth-p t)
               (:rate-limited-p nil))

(defapi%post admin%check-username-is-available ("username_avaiable")
             "Checks to see if a username is available, and valid, for the server. See the client-server API for more information."
             ((username
               :accessor username
               :initarg :username
               :query-param-p t
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))




























