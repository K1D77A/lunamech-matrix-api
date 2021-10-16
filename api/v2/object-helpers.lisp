(in-package #:lunamech-matrix-api/v2)

(defun %quick-hash (alist &rest rest &key &allow-other-keys)
  "Takes in an alist and quickly generators a hash"
  (let ((hashtable (apply #'make-hash-table rest)))
    (mapc (lambda (alist)
            (destructuring-bind (a . b)
                alist
              (unless (eq b :ne)
                (setf (gethash a hashtable) b))))
          alist)
    hashtable))

(defun %clean-alist (alist)
  (loop :for (a . b) :on alist
        :when (not (eq b :ne))
          :appending (cons a b)))

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

(defun object%image-info (&key (h :ne) (w :ne) (mimetype :ne)
                            (size :ne) (thumbnail-url :ne) (thumbnail-file :ne)
                            (thumbnail-info :ne))
  (%quick-hash `(("h" . ,h)("w" . ,w)("mimetype" . ,mimetype)("size" . ,size)
                 ("thumbnail_url" . ,thumbnail-url)("thumbnail_file" . ,thumbnail-file)
                 ("thumbnail_info" . ,thumbnail-info))))

(defun object%file-info (&key (mimetype :ne) (size :ne) (thumbnail-url :ne)
                           (thumbnail-file :ne)
                           (thumbnail-info :ne))
  (%quick-hash `(("mimetype" . ,mimetype)("size" . ,size)
                 ("thumbnail_url" . ,thumbnail-url)("thumbnail_file" . ,thumbnail-file)
                 ("thumbnail_info" . ,thumbnail-info))))

(defun object%thumbnail-info (&key (h :ne) (w :ne) (mimetype :ne) (size :ne))
  (%quick-hash `(("h" . ,h)("w" . ,w)("mimetype" . ,mimetype)("size" . ,size))))

(defun object%event/m-room-message/m-image (&key (body :ne) (info :ne) (url :ne) (file :ne))
  (or (or url file) (error "url must be set if not encrypted, file if encrypted."))
  (values (%quick-hash `(("body" . ,body)
                         ("info" . ,info)
                         ("url" . ,url)
                         ("msgtype" . "m.image")
                         ("file" . ,file)))
          "m.room.message"))

(defun object%event-filter (&key (limit :ne) (not-senders :ne)
                              (not-types :ne) (senders :ne)
                              (types :ne))
  (%quick-hash `(("limit" . ,limit) ("not_senders" . ,not-senders)
                 ("not_types" . ,not-types)("senders" . ,senders)
                 ("types" . ,types))))

(defun object%room-filter (&key (not-rooms :ne)(rooms :ne)(ephemeral :ne)
                             (include-leave :ne)(state :ne)(timeline :ne)
                             (account-data :ne))
  (%quick-hash `(("not_rooms" . ,not-rooms)("rooms" . ,rooms)("ephemeral" . ,ephemeral)
                 ("include_leave" . ,include-leave)("state" . ,state)("timeline" . ,timeline)
                 ("account_data" . ,account-data))))

(defun object%state-filter (&key (limit :ne) (not-senders :ne)
                              (not-types :ne) (senders :ne)
                              (types :ne) (lazy-load-members :ne)
                              (include-redundant-members :ne)
                              (not-rooms :ne)
                              (rooms :ne)
                              (contains-url :ne))
  (%quick-hash `(("limit" . ,limit) ("not_senders" . ,not-senders)
                 ("not_types" . ,not-types)("senders" . ,senders)
                 ("types" . ,types)("not_rooms" . ,not-rooms)("rooms" . ,rooms)
                 ("contains_url" . ,contains-url)("lazy_load_members" . ,lazy-load-members)
                 ("include_redundant_members" . ,include-redundant-members))))

(defun object%room-event-filter (&rest keys &key &allow-other-keys)
  "See object%state-filter for options."
  (apply #'object%state-filter keys))



