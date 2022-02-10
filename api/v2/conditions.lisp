(in-package #:lunamech-matrix-api/v2)

(define-condition lunamech-matrix-api-condition (error)
  ())

(define-condition api-error (lunamech-matrix-api-condition)
  ((api-error-error
    :accessor api-error-error
    :initarg :api-error-error
    :initform "default"
    :type string)
   (api-error-code
    :accessor api-error-code
    :initarg :api-error-code
    :initform "default"
    :type string)
   (api-error-args
    :accessor api-error-args
    :initform "default"
    :initarg :api-error-args)
   (api-error-description
    :accessor api-error-description
    :initarg :api-error-description
    :initform "default"))
  (:report
   (lambda (api-error stream)
     (format stream "~&Error code: ~S~%Error value: ~S~%Description: ~S~%~A"
             (api-error-code api-error)
             (api-error-error api-error)
             (api-error-description api-error)
             (when (api-error-args api-error)
               (format nil "Args: ~A~%" (api-error-args api-error)))))))

(define-condition api-timeout (api-error)
  ((api-timeout-message
    :accessor api-timeout-message
    :initarg :api-timeout-message)
   (api-timeout-condition
    :accessor api-timeout-condition
    :initarg :api-timeout-condition))
  (:report
   (lambda (con stream)
     (format stream "Connection Timeout.~%Message: ~A~%Original condition: ~A~%"
             (api-timeout-message con)
             (api-timeout-condition con)))))

(define-condition api-request-failed (api-error)
  ((api-request-failed-message
    :accessor api-request-failed-message
    :initarg :api-request-failed-message)
   (api-request-failed-condition
    :accessor api-request-failed-condition
    :initarg :api-request-failed-condition))
  (:report
   (lambda (con stream)
     (format stream "HTTP request failed.~%Message: ~A~%Original condition: ~A~%"
             (api-timeout-message con)
             (api-timeout-condition con)))))

(define-condition api-no-connection (api-timeout)
  ())

(defparameter *string->condition* (make-hash-table :test #'equal))

(defun add-string->condition (string condition-sym)
  (setf (gethash string *string->condition*) condition-sym))

(defun get-string->condition (string)
  (gethash string *string->condition* 'api-error))

(defmacro new-matrix-condition (name &body description)
  `(progn (define-condition ,name (api-error)
            ((api-error-description :initform ,@description)))
          (add-string->condition ,(string-upcase (str:snake-case (string-upcase name)))
                                 ',name)))

(new-matrix-condition m-forbidden 
    "Forbidden access, e.g. joining a room without permission, failed login.")

(new-matrix-condition m-unknown-token 
  "The access token specified was not recognised.
An additional response parameter, soft_logout, might be present on the response for 401 HTTP status codes. See the soft logout section for more information.")

(new-matrix-condition m-missing-token 
  "No access token was specified for the request.")

(new-matrix-condition m-bad-json 
  "Request contained valid JSON, but it was malformed in some way, e.g. missing required keys, invalid values for keys.")

(new-matrix-condition m-not-json 
  "Request did not contain valid JSON.")

(new-matrix-condition m-not-found 
  "No resource was found for this request.")

(new-matrix-condition m-limit-exceeded 
  "Too many requests have been sent in a short period of time. Wait a while then try again.")

(new-matrix-condition m-unknown 
  "An unknown error has occurred.")

(new-matrix-condition m-unrecognized 
  "The server did not understand the request.")

(new-matrix-condition m-unauthorized 
  "The request was not correctly authorized. Usually due to login failures.")

(new-matrix-condition m-invalid-param 
  "A parameter is invalid")

(new-matrix-condition m-room-in-use 
  "Attempting to a use a room that already exists")

(new-matrix-condition m-bad-state 
  "The state change requested cannot be performed, such as attempting to unban a user who is not banned")

(defun signal-condition-from-response (response)
  (with-hash-keys (|errcode| |error| |retry_after_ms|)
      response
    (error (get-string->condition |errcode|)
           :api-error-code |errcode|
           :api-error-error |error|
           :api-error-args |retry_after_ms|)))

