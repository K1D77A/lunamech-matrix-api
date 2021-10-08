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
               :one-of '("true" "false")
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
              :one-of '("b" "f")
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
               :requiredp t))
             (:api "/_synapse/admin/v1/")
             (:requires-auth-p t)
             (:rate-limited-p nil))








