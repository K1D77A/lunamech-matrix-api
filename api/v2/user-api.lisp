(in-package #:lunamech-matrix-api/v2)






(defapi%post message-room ("rooms/:room-id/send/:event-type/:txn")
             "Sends message event to :room-id"
             ((room-id
               :initarg :room-id
               :in-url-p t)
              (event-type
               :accessor event-type
               :in-url-p t
               :requiredp t
               :initform "m.room.message"
               :initarg :event-type
               :encoder nil)
              (body 
               :accessor body
               :initarg :body
               :requiredp t
               :specialp t)
              (txn
               :accessor txn
               :initarg :txn
               :requiredp nil
               :encoder nil
               :in-url-p t))
             (:requires-auth-p nil)
             (:rate-limited-p nil)
             (:specialp t))

