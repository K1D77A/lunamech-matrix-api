(in-package #:lunamech-matrix-api/v2)

(defapi%get client-versions%2 ("versions")
            "Gets the versions of the specification supported by the server."
            ();tested
            (:rate-limited-p nil)
            (:requires-auth-p nil)
            (:api "/_matrix/client/"))


(defapi%get discover-domain-info%4 ("")
            "Gets discovery information about the domain."
            ();tested
            (:rate-limited-p nil)
            (:requires-auth-p nil)
            (:api "/.well-known/matrix/client"))

(defapi%get get-supported-logins%5 ("login")
            "Gets the homeserver's supported login types to authenticate users. Clients should pick one of these and supply it as the type when logging in."
            ();tested 
            (:requires-auth-p nil))

(defapi%post login-connection%5 ("login")
             "Authenticates the user, and issues an access token they can use to authorize themself in subsequent requests."
             ;;pw login tested
             ((login-type
               :accessor login-type
               :initform "m.login.password"
               :name->json :type
               :requiredp t)
              (identifier
               :accessor identifier
               :initarg :identifier
               :requiredp t)
              (password
               :accessor password
               :initarg :password)
              (initial-device-display-name
               :accessor initial-device-display-name
               :initarg :initial-device-display-name)
              (device-id
               :accessor device-id
               :initarg :device-id))
             (:requires-auth-p nil))

(defapi%post logout-connection%5 ("logout")
             "Invalidates an existing access token, so that it can no longer be used for authorization."
             ();tested
             (:rate-limited-p nil))

(defapi%post logout-all-connection%5 ("logout/all")
             "Invalidates all access tokens for a user, so that they can no longer be used for authorization."
             ()
             (:rate-limited-p nil))

(defapi%post register-account%5 ("register")
             "Register for an account on this homeserver."
             ((kind
               :accessor kind
               :initarg :kind
               :name->json "kind"
               :initform "user"
               :query-param-p t)
              (auth
               :accessor auth
               :initarg :auth
               :requiredp t)
              (username
               :accessor username
               :initarg :username)
              (password
               :accessor password
               :initarg :password)
              (device-id
               :accessor device-id
               :initarg :device-id)
              (initial-device-display-name
               :accessor initial-device-display-name
               :initarg :initial-device-display-name)
              (inhibit-login
               :accessor inhibit-login
               :initarg :inhibit-login
               :initform "false"))
             (:rate-limited-p t)
             (:requires-auth-p nil))


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

