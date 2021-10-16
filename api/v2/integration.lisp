(in-package #:lunamech-matrix-api/v2)

(defclass open-id-object ()
  ((access-token
    :reader access-token
    :initarg :access-token)
   (token-type
    :reader token-type
    :initarg :token-type)
   (matrix-server-name
    :reader matrix-server-name
    :initarg :matrix-server-name)
   (ticker
    :reader ticker
    :initarg :ticker
    :documentation "")
   (expires-in
    :reader expires-in
    :initarg :expires-in)))

(defclass dimension-connection (connection)
  ())

(defclass dimension-api (api)
  ()
  (:metaclass api-call))

(defmacro defintegration-api (name (endpoint) docstring slots
                              &rest class-options)
  `(progn (let ((class (defclass ,name (dimension-api)
                         ,slots
                         ,@(append `((:metaclass api-call)                        
                                     (:endpoint ,endpoint)
                                     (:documentation ,docstring))
                            class-options))))
            (c2mop:ensure-finalized class))))

(defmacro defintegration-api%post (name (endpoint) docstring slots &rest class-options)
  `(defintegration-api ,name (,endpoint) ,docstring ,slots
                       ,@(append class-options
                                 '((:request-fun dexador:post)))))

(defmacro defintegration-api%get (name (endpoint) docstring slots &rest class-options)
  `(defintegration-api ,name (,endpoint) ,docstring ,slots
                       ,@(append class-options
                                 '((:request-fun dexador:get)))))

(defintegration-api%post integration%register ("/_matrix/integrations/v1/account/register")
                         "Exchanges an OpenID object for a token which can be used to authorize future requests to the manager."
                         ((access-token
                           :accessor access-token
                           :initarg :access-token
                           :requiredp t)
                          (token-type
                           :accessor token-type
                           :initarg :token-type
                           :requiredp t)
                          (matrix-server-name
                           :accessor matrix-server-name
                           :initarg :matrix-server-name
                           :requiredp t)
                          (expires-in
                           :accessor expires-in
                           :initarg :expires-in
                           :requiredp t))
                         (:requires-auth-p nil)
                         (:rate-limited-p t))

(defintegration-api%get integration%account ("/_matrix/integrations/v1/account")
                        "Gets information about the token's owner, such as the user ID for which it belongs."
                        ()
                        (:requires-auth-p t)
                        (:rate-limited-p t))

(defmethod generate-url ((api dimension-api))
  (let ((connection (connection api)))
    (format nil "~A~A" (url connection) (endpoint api))))

(defmethod call-api ((api dimension-api))
  (symbol-macrolet ((fun (request-fun api))
                    (connection (connection api)))
    (with-accessors ((url url)
                     (auth auth))
        connection
      (when (requires-auth-p api)
        (or (slot-boundp auth 'token)
            (error "Token has to be set.")))
      (let ((url (generate-url api))
            (header-list (generate-header-list api fun (generate-body api))))
        (setf (result api) (jojo:parse
                            (execute-api-call api fun url header-list)))
        (values (result api) api)))))

(defun open-id-plist->register-request (openid-plist dimension-connection)
  (destructuring-bind (&key |expires_in| |matrix_server_name| |token_type|
                         |access_token| &allow-other-keys)
      openid-plist
    (make-instance 'integration%register
                   :connection dimension-connection :token-type |token_type|
                   :expires-in |expires_in| :matrix-server-name |matrix_server_name|
                   :access-token |access_token|)))

(defun integration%register (dimension-connection openid-plist)
  (let* ((call (open-id-plist->register-request openid-plist dimension-connection))
         (res (call-api call))
         (auth (make-instance 'auth :token (getf res :|token|))))
    (setf (auth dimension-connection) auth)
    dimension-connection))

(defun integration%user-information (dimension-connection)
  (or (slot-boundp (auth dimension-connection) 'token)
      (error "You have not called integration%register yet."))
  (call-api (make-instance 'integration%account :connection dimension-connection)))

(defun integration%validate-user-id (dimension-connection open-id-plist user-id)
  (integration%register dimension-connection open-id-plist)
  (string= user-id (getf (integration%user-information dimension-connection) :|user_id|)))
