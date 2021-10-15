(in-package #:lunamech-matrix-api/v2)

(defapi%post filters%upload ("user/:user-id/filter")
             "Uploads a new filter definition to the homeserver. Returns a filter ID that may be used in future requests to restrict which events are returned to the client."
             ((user-id
               :accessor user-id
               :initarg :user-id
               :requiredp t
               :in-url-p t
               :name->json "userId")
              (event-fields
               :accessor event-fields
               :initarg :event-fields)
              (event-format
               :accessor event-format
               :initarg :event-format
               :initform "client"
               :one-of '("client" "federation"))
              (presence
               :accessor presence
               :initarg :presence)
              (account-data
               :accessor account-data
               :initarg :account-data)
              (room-filter
               :accessor room-filter
               :initarg :room-filter
               :name->json "room"))
             (:requires-auth-p t)
             (:rate-limited-p nil))

(defapi%get filters%download ("user/:user-id/filter/:filter-id")
            "Download a filter"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :requiredp t
              :in-url-p t
              :name->json "userId")
             (filter-id
              :accessor filter-id
              :initarg :filter-id
              :requiredp t
              :in-url-p t
              :name->json "filterId"))
            (:requires-auth-p t)
            (:rate-limited-p nil))
