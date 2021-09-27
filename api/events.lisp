
(in-package #:lunamech-matrix-api)

;; (defparameter *test-event*
;;   (make-event (make-instance 'm-text :body "im a test event")
;;               "m.room.message"
;;               "$syZqu6MpkHPcMP1AVYSBNj51I1MWnMwkB33W_DHhzVc"
;;               "@k1d77a:scyldings.com"
;;               (format nil "~D" (get-universal-time))
;;               "!WJvFXSrAnfoqNgwqpE:scyldings.com"))

(defclass event ()
  ((content
    :accessor content
    :initarg :content)
   (etype
    :accessor etype
    :initform "m.room.message"
    :initarg :etype)
   (event-id
    :accessor event-id
    :initarg :event-id)
   (sender
    :accessor sender
    :initarg :sender)
   (origin-server-ts
    :accessor origin-server-ts
    :initarg :origin-server-ts)
   (unsigned
    :accessor unsigned
    :initarg :unsigned)
   (room-id
    :accessor room-id
    :initarg :room-id)))

(defun make-event (content etype event-id sender origin-server-ts room-id
                   &optional (unsigned nil))
  (make-instance 'event :content content :etype etype :event-id event-id
                        :origin-server-ts origin-server-ts :room-id room-id
                        :unsigned unsigned :sender sender))

(defmethod jojo:%to-json ((event event))
  (jojo:with-object
    (jojo:write-key-value "content" (slot-value event 'content))
    (jojo:write-key-value "type" (slot-value event 'etype))
    (jojo:write-key-value "event_id" (slot-value event 'event-id))
    (jojo:write-key-value "sender" (slot-value event 'sender))
    (jojo:write-key-value "origin_server_ts" (slot-value event 'origin-server-ts))
    (jojo:write-key-value "room_id" (slot-value event 'room-id))
    (when (slot-value event 'unsigned)
      (jojo:write-key-value "unsigned" (slot-value event 'unsigned)))))

(defclass m-message ()
  ((body
    :accessor body
    :initarg :body)
   (msgtype
    :accessor msgtype
    :initarg :msgtype)))

(defmethod jojo:%to-json ((m-message m-message))
  (jojo:with-object    
    (jojo:write-key-value "msgtype" (slot-value m-message 'msgtype))
    (jojo:write-key-value "body" (slot-value m-message 'body))))

(defclass m-text (m-message)
  ((msgtype
    :initform "m.text")
   (mformat
    :accessor mformat
    :initarg :mformat 
    :initform nil)
   ;;canbe "org.matrix.custom.html"
   (formatted-body
    :accessor formatted-body
    :initarg :formatted-body
    :initform nil)))

(defmethod jojo:%to-json ((m-text m-text))
  (with-slots (msgtype body mformat formatted-body)
      m-text
    (jojo:with-object    
      (jojo:write-key-value "msgtype" msgtype)
      (jojo:write-key-value "body" body)
      (when (string= mformat "org.matrix.custom.html")
        (jojo:write-key-value "format" mformat)
        (jojo:write-key-value "formatted_body" formatted-body)))))

(defclass m-image (m-message)
  ((msgtype
    :initform "m.image")
   (url
    :initarg :url)))

(defclass m-file (m-message)
  ((msgtype
    :initform "m.file")
   (url
    :initarg :url)
   (info
    :initarg :info
    :type file-info)))

(defclass file-info ()
  ((mimetype
    :accessor mimetype
    :initarg :mimetype
    :type string)
   (size
    :initarg :size
    :type integer)))

(defmethod jojo:%to-json ((m-image m-image))
  (jojo:with-object
    (jojo:write-key-value "msgtype" (slot-value m-image 'msgtype))
    (jojo:write-key-value "body" (slot-value m-image 'body))
    (jojo:write-key-value "url" (slot-value m-image 'url))))

(defmethod jojo:%to-json ((obj file-info))
  (jojo:with-object
    (jojo:write-key-value "mimetype" (slot-value obj 'mimetype))
    (jojo:write-key-value "size" (slot-value obj 'size))))

(defmethod jojo:%to-json ((obj m-file))
  (jojo:with-object
    (jojo:write-key-value "msgtype" (slot-value obj 'msgtype))
    (jojo:write-key-value "info" (slot-value obj 'info))
    (jojo:write-key-value "body" (slot-value obj 'body))
    (jojo:write-key-value "url" (slot-value obj 'url))))

(defclass container ()
  ((contain
    :accessor contain
    :initarg :contain)))

(defmethod jojo:%to-json ((obj container))
  (jojo:with-object
    (jojo:write-key "content")
    (jojo:write-value (dolist (i (slot-value obj 'contain))
                        (jojo:with-object
                          (jojo:write-key (first i))
                          (jojo:write-value (second i)))
                        (jojo:%write-char #\,)))
    ))

(defclass m-direct ()
  ((content
    :accessor content
    :initarg :content
    :type list)
   (etype
    :accessor etype
    :initform "m.direct")))

(defclass direct-content ()
  ((user-id
    :accessor user-id
    :initarg :user-id
    :type string)
   (room-ids
    :accessor room-ids
    :initarg :room-ids
    :type list)))

(defmethod jojo:%to-json ((obj m-direct))
  (jojo:with-object
    (jojo:write-key-value "content" (slot-value obj 'content))
    (jojo:write-key-value "type" (slot-value obj 'etype))))

(defmethod jojo:%to-json ((obj direct-content))
  (jojo:with-object
    (jojo:write-key (slot-value obj 'user-id))
    (jojo:write-value (slot-value obj 'room-ids))))

(defclass event-content ()
  ((content
    :accessor content
    :initarg :content)))

(defclass m-room-name ()
  ((name
    :accessor name
    :initarg :name
    :type string)))

(defmethod jojo:%to-json ((obj event-content))
  (jojo:with-object
    (jojo:write-key-value "content" (slot-value obj 'content))))

(defmethod jojo:%to-json ((obj m-room-name))
  (jojo:with-object
    (jojo:write-key-value "type" "m.room.name")
    (jojo:write-key-value "name" (slot-value obj 'name))))

(defclass m-presence ()
  ((avatar-url
    :accessor avatar-url
    :initarg :avatar-url
    :type string)
   (presence
    :accessor presence
    :initarg :presence
    :initform "online")))

(defmethod jojo:%to-json ((obj m-presence))
  (jojo:with-object
    (jojo:write-key-value "avatar_url" (slot-value obj 'avatar-url))
    (jojo:write-key-value "presence" (slot-value obj 'presence))))

(defclass power-level ()
  ((users
    :accessor users
    :initarg :users
    :type power-level%users
    :documentation "A power-level%users object")))

(defclass power-level%users ()
  ((users
    :accessor users
    :initarg :users
    :initform ()
    :documentation "An alist of users to their powerlevels. 50 is moderator, 100 admin.")))

(defmethod jojo:%to-json ((obj power-level))
  (jojo:with-object
    (jojo:write-key-value "content"
                          (jojo:%to-json (slot-value obj 'users)))))

(defmethod jojo:%to-json ((obj power-level%users))
  (jojo:with-object 
    (jojo:write-key-value "users" (jojo:to-json (slot-value obj 'users) :from :alist))))




