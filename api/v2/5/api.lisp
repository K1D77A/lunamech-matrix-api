(in-package #:lunamech-matrix-api/v2)

(defapi%get get-supported-logins ("login")
  "Gets the homeserver's supported login types to authenticate users. Clients should pick one of these and supply it as the type when logging in."
  ();tested 
  (:requires-auth-p nil))

(defapi%post login-connection ("login")
             "Authenticates the user, and issues an access token they can use to authorize themself in subsequent requests."
             ;;pw login tested
             ((login-type
               :accessor login-type
               :initarg :login-type
               :initform "m.login.password"
               :name->json "type"
               :requiredp t
               :one-of '("m.login.password" "m.login.token"))
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

(defapi%post logout-connection ("logout")
             "Invalidates an existing access token, so that it can no longer be used for authorization."
             ();tested
             (:rate-limited-p nil))

(defapi%post logout-all-connection ("logout/all")
             "Invalidates all access tokens for a user, so that they can no longer be used for authorization."
             ()
             (:rate-limited-p nil))

(defapi%post register-account ("register")
             "Register for an account on this homeserver."
             ((kind
               :accessor kind
               :initarg :kind
               :name->json "kind"
               :initform "user"
               :query-param-p t
               :one-of '("guest" "user"))
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
               :initform nil))
             (:rate-limited-p t)
             (:requires-auth-p nil))

(defapi%post register-account/email/request-token ("register/email/requestToken")
             "The homeserver must check that the given email address is not already associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (email
               :accessor email
               :initarg :email
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link)
              (id-server
               :accessor id-server
               :initarg :id-server)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token))
             (:rate-limited-p nil)
             (:requires-auth-p nil))

(defapi%post register-account/msisdn/request-token ("register/msisdn/requestToken")
             "The homeserver must check that the given phone number is not already associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (country
               :accessor country
               :initarg :country
               :requiredp t)
              (phone-number
               :accessor phone-number
               :initarg :phone-number
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link)
              (id-server
               :accessor id-server
               :initarg :id-server)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token))
             (:rate-limited-p nil)
             (:requires-auth-p nil))

(defapi%post change-password ("account/password")
             "Changes the password for an account on this homeserver."
             ((new-password
               :accessor new-password
               :initarg :new-password
               :requiredp t)
              (logout-devices
               :accessor logout-devices
               :initarg :logout-devices
               :initform t)
              (auth
               :accessor auth
               :initarg :auth
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post change-password/email/request-token ("account/password/email/requestTOken")
             "The homeserver must check that the given email address is associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (email
               :accessor email
               :initarg :email
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link)
              (id-server
               :accessor id-server
               :initarg :id-server)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post change-password/msisdn/request-token ("account/password/msisdn/requestToken")
             "The homeserver must check that the given phone number is not already associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (country
               :accessor country
               :initarg :country
               :requiredp t)
              (phone-number
               :accessor phone-number
               :initarg :phone-number
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link))
             (:rate-limited-p nil)
             (:requires-auth-p nil))

(defapi%post deactivate-account ("account/deactivate")
             "Deactivate the user's account, removing all ability for the user to login again."
             ((id-server
               :accessor id-server
               :initarg :id-server)
              (auth
               :accessor auth
               :initarg :auth
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%get username-available ("register/available")
            "Checks to see if a username is available, and valid, for the server."
            ((username
              :accessor username
              :initarg :username
              :name->json "username"
              :query-param-p t
              :requiredp t))
            (:rate-limited-p t)
            (:requires-auth-p nil))

(defapi%get 3pid%get ("account/3pid")
            "Gets a list of the third party identifiers that the homeserver has associated with the user's account."
            ()
            (:rate-limited-p nil)
            (:requires-auth-p t))

(defapi%post 3pid%add ("account/3pid/add")
             "Adds contact information to the user's account. Homeservers should use 3PIDs added through this endpoint for password resets instead of relying on the identity server."
             ((auth
               :accessor auth
               :initarg :auth
               :requiredp t)
              (client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (sid
               :accessor sid
               :initarg :sid
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post 3pid%bind ("account/3pid/bind")
             "Binds a 3PID to the user's account through the specified identity server."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (id-server
               :accessor id-server
               :initarg :id-server
               :requiredp t)
              (sid
               :accessor sid
               :initarg :sid
               :requiredp t)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token
               :requiredp t))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%post 3pid%delete ("account/3pid/delete")
             "Removes a third party identifier from the user's account. This might not cause an unbind of the identifier from the identity server."
             ((id-server
               :accessor id-server
               :initarg :id-server
               :requiredp nil)
              (medium
               :accessor medium
               :initarg :medium
               :requiredp t
               :one-of '("email" "msisdn"))
              (address
               :accessor address
               :initarg :address
               :requiredp t))             
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post 3pid%unbind ("account/3pid/unbind")
             "Removes a user's third party identifier from the provided identity server without removing it from the homeserver."
             ((id-server
               :accessor id-server
               :initarg :id-server
               :requiredp nil)
              (medium
               :accessor medium
               :initarg :medium
               :one-of '("email" "msisdn")
               :requiredp t)
              (address
               :accessor address
               :initarg :address
               :requiredp t))             
             (:rate-limited-p nil)
             (:requires-auth-p t))

(defapi%post 3pid%email/request-token ("account/3pid/email/requestToken")
             "The homeserver must check that the given email address is not already associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (email
               :accessor email
               :initarg :email
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link)
              (id-server
               :accessor id-server
               :initarg :id-server)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token))
             (:rate-limited-p nil)
             (:requires-auth-p nil))

(defapi%post 3pid%msisdn/request-token ("account/3pid/msisdn/requestToken")
             "The homeserver must check that the given phone number is not already associated with an account on this homeserver."
             ((client-secret
               :accessor client-secret
               :initarg :client-secret
               :requiredp t)
              (country
               :accessor country
               :initarg :country
               :requiredp t)
              (phone-number
               :accessor phone-number
               :initarg :phone-number
               :requiredp t)
              (send-attempt
               :accessor send-attempt
               :initarg :send-attempt
               :requiredp t)
              (next-link
               :accessor next-link
               :initarg :next-link)
              (id-server
               :accessor id-server
               :initarg :id-server)
              (id-access-token
               :accessor id-access-token
               :initarg :id-access-token))
             (:rate-limited-p nil)
             (:requires-auth-p nil))

(defapi%get whoami ("account/whoami")
            "Gets information about the owner of a given access token."
            ()
            (:rate-limited-p t)
            (:requires-auth-p t))


