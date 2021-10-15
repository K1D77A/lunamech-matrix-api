(in-package #:lunamech-matrix-api/v2)

(defapi%get sync ("sync")
            "Sync"
            ((filter
              :accessor filter
              :initarg :filter
              :query-param-p t
              :requiredp nil)
             (since
              :accessor since
              :initarg :since
              :query-param-p t
              :requiredp nil)
             (full-state
              :accessor full-state
              :initarg :full-state
              :initform "false"
              :query-param-p t
              :requiredp nil)
             (set-presence
              :accessor set-presence
              :initarg :set-presence
              :query-param-p t
              :requiredp nil
              :one-of '("offline" "online" "unavailable"))
             (timeout
              :accessor timeout
              :initarg :timeout
              :initform 0
              :query-param-p t
              :requiredp nil))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get events%get-from-id ("rooms/:room-id/event/:event-id")
            "Get a single event based on roomId/eventId. You must have permission to retrieve this event e.g. by being a member in the room for this event."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (event-id
              :accessor event-id
              :initarg :event-id
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get events%get-from-type-with-statekey
    ("rooms/:room-id/event/:event-type/:state-key")
    "Looks up the contents of a state event in a room. If the user is joined to the room then the state is taken from the current state of the room. If the user has left the room then the state is taken from the state of the room when they left."
    ((room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (event-type
      :accessor event-type
      :initarg :event-type
      :requiredp t
      :in-url-p t)
     (state-key
      :accessor state-key
      :initarg :state-key
      :initform ""
      :requiredp t
      :in-url-p t))
    (:rate-limited-p nil)
    (:requires-auth-p t))

(defapi%get events%get-state-events-in-room ("rooms/:room-id/state")
            "Get the state events for the current state of a room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get events%get-room-members ("rooms/:room-id/members")
            "Get the list of members for this room"
            ((room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (at
              :accessor at
              :initarg :at
              :query-param-p t)
             (membership
              :accessor membership
              :initarg :membership
              :query-param-p t
              :one-of '("join" "invite" "leave" "ban"))
             (not-membership
              :accessor not-membership
              :initarg :not-membership
              :query-param-p t
              :one-of '("join" "invite" "leave" "ban")))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get events%get-joined-members ("rooms/:room-id/joined_members")
            "This API returns a map of MXIDs to member info objects for members of the room. The current user must be in the room for it to work, unless it is an Application Service in which case any of the AS's users must be in the room. This API is primarily for Application Services and should be faster to respond than /members as it can be implemented more efficiently on the server."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%get events%get-room-messages ("rooms/:room-id/messages")
            "This API returns a list of message and state events for a room. It uses pagination query parameters to paginate history in the room."
            ((room-id
              :accessor room-id
              :initarg :room-id
              :requiredp t
              :in-url-p t)
             (from
              :accessor from
              :initarg :from
              :requiredp t
              :query-param-p t)
             (to
              :accessor to 
              :initarg :to
              :query-param-p t)
             (dir 
              :accessor dir
              :initarg :dir
              :requiredp t
              :query-param-p t)
             (limit
              :accessor limit
              :initarg :limit
              :initform 10 
              :requiredp nil
              :query-param-p t)
             (filter
              :accessor filter
              :initarg :filter
              :requiredp nil
              :query-param-p nil))
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%put events%put-state-event-into-room 
    ("rooms/:room-id/state/:event-type/:state-key")
    "State events can be sent using this endpoint. These events will be overwritten if <room id>, <event type> and <state key> all match."
    ((room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (event-type
      :accessor event-type
      :initarg :event-type
      :requiredp t
      :in-url-p t)
     (state-key
      :accessor state-key
      :initarg :state-key
      :initform ""
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

(defapi%put events%put-message-event-into-room  
    ("rooms/:room-id/send/:event-type/:txn")
    "This endpoint is used to send a message event to a room. Message events allow access to historical events and pagination, making them suited for \"once-off\" activity in a room."
    ((room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (event-type
      :accessor event-type
      :initarg :event-type
      :requiredp t
      :in-url-p t)
     (txn 
      :accessor txn
      :initarg :txn
      :requiredp nil
      :in-url-p t)
     (body
      :accessor body
      :initarg :body
      :requiredp t
      :specialp t))
    (:specialp t)
    (:rate-limited-p nil)
    (:requires-auth-p t))

(defapi%put events%redact-event 
    ("rooms/:room-id/redact/:event-id/:txn")
    "Strips all information out of an event which isn't critical to the integrity of the server-side representation of the room."
    ((room-id
      :accessor room-id
      :initarg :room-id
      :requiredp t
      :in-url-p t)
     (event-id
      :accessor event-id
      :initarg :event-id
      :requiredp t
      :in-url-p t)
     (txn 
      :accessor txn
      :initarg :txn
      :requiredp t
      :in-url-p t)
     (reason
      :accessor reason 
      :initarg :reason
      :initform "Event redacted"
      :requiredp t))
    (:rate-limited-p nil)
    (:requires-auth-p t))

