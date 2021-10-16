(in-package #:matrix-moonbot)
;;;;this file contains the code for creating and managing filters within Moonbot.

(defun get-filter (instance key)
  (find key (filters instance) :key #'key))

(defun add-filter-id (instance key new-val &optional (last-sync nil))
  (check-type last-sync (or null string))
  (let* ((filter (find key (filters instance) :key #'key)))
    (if filter 
        (setf (id filter) new-val)
        (push (make-instance 'filter :id new-val :key key
                                     :last-sync-string last-sync)
              (filters instance)))))

(defun filter-to-remove-junk ()
  `(("event_format" "client")
    ("presence" ("not_types" ("m.presence")))
    ("room" ("ephemeral" ("not_types" ("m.room.*" "m.receipt"
                                                  "m.typing" "m.reaction"))))))
                                        ;ignore all ephemeral

(defmethod generate-filter-for-invites (list-of-inviters)
  `(("event_format" "client")
    ("presence" ("not_types" ("m.presence")))
    ("room" ("state" ("types" ("m.room.member")) ("sender" ,list-of-inveters)
                     ("not_types" ("m.room.message" "m.room.encrypted")))
            ("timeline" ("types" ("m.room.member"))
                        ("not_types" ("m.room.message" "m.room.encrypted"))))))

(defmethod generate-filter-for-messages-in-room-list (list)
  `(("event_format" "client")
    ("presence" ("not_types" ("m.presence")))
    ("room" ("rooms" ,list)
            ("state" ("types" ("m.room.message" "m.room.encrypted")))
            ("timeline" ("rooms" ,list)
                        ("types" ("m.room.message" "m.room.encrypted"))))))

(defun filter-to-remove-receipts-reaction-typing (connection user-id)
  (generate-user-room-filter connection user-id
                             :event-format "client"
                             :presence (object%event-filter :not-types '("m.presence"))
                             :room-filter
                             (object%room-filter :ephemeral
                                                 (object%room-event-filter
                                                  :not-types '("m.room.*" "m.receipt"
                                                               "m.typing" "m.reaction")))))

(defun generate-user-room-filter (connection user-id &rest keys &key &allow-other-keys)
  (apply #'make-instance 'filters%upload
         (append (list :connection connection :user-id user-id)
                 keys)))


(defmethod add-user-room-filter (connection user-id (filter filter-request))
  (make-instance 'filters%upload )
  (auth-req (:post-object connection ("user/" user-id "/filter")
             (jojo:to-json filter)
             resp)
    resp))

(defmethod add-user-room-filter (connection user-id (filter list))
  (let ((filter (filter-list->filter-request filter)))
    (add-user-room-filter connection user-id filter)))

(defmethod generate-and-upload-junk-removal-filter ((connection connection))
  (let* ((filter (filter-to-remove-junk))
         (filter-id (pkv (add-user-room-filter connection (user-id connection) filter)
                         :|filter_id|)))
    (add-filter-id connection :junk-removed filter-id)))

