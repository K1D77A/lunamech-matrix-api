(in-package #:lunamech-matrix-api/v2)
;;;;this file contains the code for creating and managing filters within Moonbot.

(defun get-filter (instance key)
  (find key (filters instance) :key #'key))

(defun add-filter-id (connection key new-val &optional (last-sync nil))
  (check-type last-sync (or null string))
  (let* ((filter (find key (filters connection) :key #'key)))
    (if filter 
        (setf (id filter) new-val)
        (push (make-instance 'filter :id new-val :key key
                                     :last-sync-string last-sync)
              (filters connection)))))


(defun filter-to-remove-receipts-reaction-typing (connection user-id)
  (generate-user-room-filter connection user-id
                             :event-format "client"
                             :presence (object%event-filter :not-types '("m.presence"))
                             :room-filter
                             (object%room-filter :ephemeral
                                                 (object%room-event-filter
                                                  :not-types '("m.room.*" "m.receipt"
                                                               "m.typing" "m.reaction")))))

(defun upload-new-filter (filter-key filter-object)
  "Uploads a new filter (FILTER-OBJECT)
 using call-api and then stores the filter_id returned from the 
call under FILTER-KEY in (filters (connection FILTER-OBJECT))."
  (let* ((res (call-api filter-object))
         (id (getf res :|filter_id|)))
    (add-filter-id (connection filter-object) filter-key id)))

(defun generate-user-room-filter (connection user-id &rest keys &key &allow-other-keys)
  (apply #'make-instance 'filters%upload
         (append (list :connection connection :user-id user-id)
                 keys)))

