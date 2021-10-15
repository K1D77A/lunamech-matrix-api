(in-package #:lunamech-matrix-api/v2)

(defun %quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators an alist"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (setf (gethash a hashtable) b)))
          alist)
    hashtable))

(defun object%identifier-type/m-id-user (user)
  (%quick-hash `(("type" . "m.id.user")("user" . ,user))))

(defun object%identifier-type/m-id-thirdparty (medium address)
  (%quick-hash `(("type" . "m.id.thirdparty")
                 ("medium" . ,medium)
                 ("address" . ,address))))

(defun object%identifier-type/m-id-phonenumber (country phone)
  (%quick-hash `(("type" . "m.id.thirdparty")
                 ("country" . ,country)
                 ("phone" . ,phone))))

(defun object%event/m-room-redaction (reason)
  (values (%quick-hash `(("reason" . ,reason))) "m.room.redaction"))

(defun object%event/m-room-message (message)
  (values (%quick-hash `(("msgtype" . "m.text")
                         ("body" . ,message)))
          "m.room.message"))

