(in-package #:lunamech-matrix-api/v2)

(defun %quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators a hash"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (when b
                (setf (gethash a hashtable) b))))
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

(defun object%event/m-room-message/m-text%basic (message)
  (values (%quick-hash `(("msgtype" . "m.text")
                         ("body" . ,message)))
          "m.room.message"))

(defun object%event/m-room-message/m-text (message formatted-body)
  (values (%quick-hash `(("msgtype" . "m.text")
                         ("body" . ,message)
                         ("format" . "org.matrix.custom.html")
                         ("formatted_body" . ,formatted-body)))
          "m.room.message"))

(defun object%image-info (&key (h nil) (w nil) (mimetype nil)
                            (size nil) (thumbnail-url nil) (thumbnail-file nil)
                            (thumbnail-info nil))
  (%quick-hash `(("h" . ,h)("w" . ,w)("mimetype" . ,mimetype)("size" . ,size)
                 ("thumbnail_url" . ,thumbnail-url)("thumbnail_file" . ,thumbnail-file)
                 ("thumbnail_info" . ,thumbnail-info))))

(defun object%file-info (&key (mimetype nil) (size nil) (thumbnail-url nil)
                           (thumbnail-file nil)
                           (thumbnail-info nil))
  (%quick-hash `(("mimetype" . ,mimetype)("size" . ,size)
                 ("thumbnail_url" . ,thumbnail-url)("thumbnail_file" . ,thumbnail-file)
                 ("thumbnail_info" . ,thumbnail-info))))

(defun object%thumbnail-info (&key (h nil) (w nil) (mimetype nil) (size nil))
  (%quick-hash `(("h" . ,h)("w" . ,w)("mimetype" . ,mimetype)("size" . ,size))))

(defun object%event/m-room-message/m.image (&key (body nil) (info nil) (url nil) (file nil))
  (or (or url file) (error "url must be set if not encrypted, file if encrypted."))
  (values (%quick-hash `(("body" . ,body)
                         ("info" . ,info)
                         ("url" . ,url)
                         ("msgtype" . "m.image")
                         ("file" . ,file)))
          "m.room.message"))

