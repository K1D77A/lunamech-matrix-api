
(in-package #:lunamech-matrix-api)

#||
Test API for Spaces
||#

(defun spaces-rooms (connection room-id)
  (auth-req (:get connection
                  ("rooms/" (url-e room-id) "/spaces")
                  nil resp :msc2946)
            resp))

(defun spaces-hierarchy (connection room-id)
  (auth-req (:get connection
             ("rooms/" (url-e room-id) "/hierarchy")
             nil resp :msc2946)
    resp))

(defun rooms-in-a-space (connection room-id)
  (let* ((response (spaces-hierarchy connection room-id))
         (rooms (getf response :|rooms|)))
    (mapcar (lambda (room)
              (destructuring-bind (&key |room_id| |name| |room_type| &allow-other-keys)
                  room
                (list :name |name| :id |room_id| :room-type |room_type|)))
            rooms)))

(defun spaces-in-a-space (connection room-id)
  (let* ((response (spaces-rooms connection room-id))
         (rooms (getf response :|rooms|)))
    (remove-if-not (lambda (room)
                     (let ((type (getf room :|room_type|)))
                       (string= type "m.space")))
                   rooms)))

(defun invite-to-space (connection user-id space-id)
  (invite-member-to-room connection user-id space-id))

(defun invite-user-to-all-spaces-in-space (connection user-id space-id)
  (let ((spaces (spaces-in-a-space connection space-id)))
    (mapc (lambda (space)
            (destructuring-bind (&key |room_id| |name| &allow-other-keys)
                space
              (format t "Inviting user to space: ~A~%" |name|)
              (handler-case 
                  (invite-member-to-room connection user-id |room_id|)
                (m-forbidden ()
                  nil))))
          spaces)))
