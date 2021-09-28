(in-package #:lunamech-matrix-api)

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

(defun make-open-id (access-token matrix-server-name expires-in
                     &optional (token-type "Bearer"))
  (make-instance 'open-id-object
                 :expires-in expires-in :matrix-server-name matrix-server-name
                 :token-type token-type :access-token access-token))

(defmethod jojo:%to-json ((ob open-id-object))
  (with-slots (access-token token-type
               matrix-server-name expires-in)
      ob
    (jojo:with-object
      (jojo:write-key-value "access_token" access-token)
      (jojo:write-key-value "token_type" token-type)
      (jojo:write-key-value "matrix_server_name" matrix-server-name)
      (jojo:write-key-value "expires_in" expires-in))))

(defun integrations-register (openid-object)
  (let ((res 
          (jojo:parse
           (dex:post (format nil "~A/_matrix/integrations/v1/account/register" *dimension*)
                     :headers `(("Content-Type" . "application/json"))
                     :content (jojo:to-json openid-object)))))
    (setf *current-token* (getf res :|token|))))

(defun integration-account (token)
  (jojo:parse
   (dex:get (format nil "~A/_matrix/integrations/v1/account" *dimension*)
            :headers `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format nil "Bearer ~A" token))))))

(defun integrations-valid-user-id-p (user-id open-id-object)
  (let* ((token (integrations/register open-id-object))
         (account (integrations/account token)))
    (string= user-id (getf account :|user_id|))))
