(in-package #:lunamech-matrix-api/v2)

(defapi message-room ("rooms/:room-id/send/:event-type/:txn" :POST)
        ((room-id
          :category :send
          :initarg :room-id
          :in-url-p t)
         (event-type
          :accessor event-type
          :category :send
          :in-url-p t
          :initarg :event-type
          :encoder nil)
         (body 
          :accessor body
          :initarg :body
          :category :send
          :specialp t)
         (txn
          :accessor txn
          :category :send
          :requiredp nil
          :encoder nil
          :in-url-p t))
        (:requires-auth-p nil)
        (:rate-limited-p nil)
        (:specialp t))



(defapi login ("login" :POST)
        ((login-type
          :accessor login-type
          :category :send
          :initform "m.login.password"
          :name->json :type
          :requiredp t)
         (identifier
          :accessor identifier
          :initarg :identifier
          :category :send)
         (password
          :accessor password
          :initarg :password
          :category :send)
         (device-id
          :accessor device-id
          :initarg :device-id
          :category :send
          :requiredp nil))
        (:requires-auth-p nil))

(defapi logout ("logout" :POST)
        ()
        (:rate-limited-p nil))
