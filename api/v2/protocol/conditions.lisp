(in-package :lunamech-matrix-api/v2)

(define-condition api-protocol-condition (serious-condition)
  ((message
    :accessor message
    :initarg :message
    :type string
    :documentation "An accompanying message."))
  (:documentation "The top level protocol condition.")
  (:report
   (lambda (obj stream)
     (format stream "Message: ~A"
             (message obj)))))

(define-condition problems-with-special (api-protocol-condition)
  ())

(define-condition set-special (problems-with-special)
  ()
  (:documentation "Signalled if you declare a slot special but do not fill it."))

(define-condition special-slot-is-not-bound (problems-with-special)
  ()
  (:documentation "Signalled when the user declares the api special but (special-slot api)
is not bound, meaning that they forgot to declare a slot special and fill it."))

(define-condition missing-required-data (api-protocol-condition)
  ((slot
    :accessor slot
    :initarg slot
    :documentation "The slot that is missing the data."))
  (:documentation "Signalled when you have tried to execute an api call but you 
are missing data from a required slot."))


