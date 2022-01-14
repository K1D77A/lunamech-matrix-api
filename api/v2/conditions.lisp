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

(define-condition m-forbidden (api-error)
  ((api-error-description
    :initform "Forbidden access, e.g. joining a room without permission, failed login.")))

(define-condition m-unknown-token (api-error)
  ((api-error-description
    :initform "The access token specified was not recognised.
An additional response parameter, soft_logout, might be present on the response for 401 HTTP status codes. See the soft logout section for more information.")))

(define-condition m-missing-token (api-error)
  ((api-error-description
    :initform
    "No access token was specified for the request.")))

(define-condition m-bad-json (api-error)
  ((api-error-description
    :initform
    "Request contained valid JSON, but it was malformed in some way, e.g. missing required keys, invalid values for keys.")))

(define-condition m-not-json (api-error)
  ((api-error-description
    :initform "Request did not contain valid JSON.")))

(define-condition m-not-found (api-error)
  ((api-error-description
    :initform "No resource was found for this request.")))

(define-condition m-limit-exceeded (api-error)
  ((api-error-description
    :initform "Too many requests have been sent in a short period of time. Wait a while then try again.")))

(define-condition m-unknown (api-error)
  ((api-error-description
    :initform "An unknown error has occurred.")))

(define-condition m-unrecognized (api-error)
  ((api-error-description
    :initform "The server did not understand the request.")))

(define-condition m-unauthorized (api-error)
  ((api-error-description
    :initform "The request was not correctly authorized. Usually due to login failures.")))

(define-condition m-invalid-param (api-error)
  ((api-error-description
    :initform "A parameter is invalid")))

(define-condition m-room-in-use (api-error)
  ((api-error-description
    :initform "Attempting to a use a room that already exists")))

(define-condition m-bad-state (api-error)
  ((api-error-description
    :initform "The state change requested cannot be performed, such as attempting to unban a user who is not banned")))

(defparameter *string->condition* (make-hash-table :test #'equal))

(defun add-string->condition (string condition-sym)
  (setf (gethash string *string->condition*) condition-sym))

(defun get-string->condition (string)
  (let ((condition (gethash string *string->condition*)))
    (unless condition
      (error (format nil "Condition for ~S not defined" string)))
    condition))

(add-string->condition "M_FORBIDDEN" 'm-forbidden)
(add-string->condition "M_UNKNOWN_TOKEN" 'm-unknown-token)
(add-string->condition "M_MISSING_TOKEN" 'm-missing-token)
(add-string->condition "M_BAD_JSON" 'm-bad-json)
(add-string->condition "M_NOT_JSON" 'm-not-json)
(add-string->condition "M_NOT_FOUND" 'm-not-found)
(add-string->condition "M_LIMIT_EXCEEDED" 'm-limit-exceeded)
(add-string->condition "M_UNKNOWN" 'm-unknown)
(add-string->condition "M_UNRECOGNIZED" 'm-unrecognized)
(add-string->condition "M_UNAUTHORIZED" 'm-unauthorized)
(add-string->condition "M_INVALID_PARAM" 'm-invalid-param)
(add-string->condition "M_ROOM_IN_USE" 'm-room-in-use)
(add-string->condition "M_BAD_STATE" 'm-bad-state)

(defun signal-condition-from-response (response)
  (with-hash-keys (|errcode| |error| |retry_after_ms|)
      response 
    (error (get-string->condition |errcode|)
           :api-error-code |errcode|
           :api-error-error |error|
           :api-error-args |retry_after_ms|)))

