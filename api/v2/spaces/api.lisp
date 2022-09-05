(in-package #:lunamech-matrix-api/v2)

(defapi%get spaces%space-hierarchy ("rooms/:room-id/hierarchy")
            "Get a spaces hierarchy"
            ((room-id
              :accessor room-id
              :initarg :room-id
              :in-url-p t
              :requiredp t)
             (suggested-only
              :accessor suggested-only
              :initarg :suggested-only
              :initform nil
              :one-of (t nil)
              :requiredp nil 
              :query-param-p t)
             (limit
              :accessor limit
              :initarg :limit
              :query-param-p t
              :requiredp nil)
             (max-depth 
              :accessor max-depth 
              :initarg :max-depth 
              :query-param-p t
              :requiredp nil)
             (from
              :accessor from 
              :initarg :from 
              :query-param-p t
              :requiredp nil))
            (:api "/_matrix/client/v1/")
            (:requires-auth-p t)
            (:rate-limited-p t))


