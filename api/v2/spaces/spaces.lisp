(in-package #:lunamech-matrix-api/v2)

(defun spaces-hierarchy (connection room-id &rest keys)
  (call-api (apply #'make-instance 'spaces%space-hierarchy
                   (append (list :room-id room-id
                                 :connection connection)
                           keys))))

(defun rooms-in-a-space (connection room-id)
  (let* ((response (spaces-hierarchy connection room-id))
         (rooms (gethash "rooms" response)))
    (mapcar (lambda (room)
              (with-hash-keys (|room_id| |name| |room_type|)
                  room
                (list :name |name| :id |room_id| :room-type |room_type|)))
            rooms)))

(defun spaces-in-a-space (connection room-id)
  (let* ((response (spaces-hierarchy connection room-id))
         (rooms (gethash "rooms" response)))
    (remove-if-not (lambda (room)
                     (let ((type (gethash "room_type" room)))
                       (string= type "m.space")))
                   rooms)))

(defun invite-to-space (connection user-id space-id)
  (invite-member-to-room connection user-id space-id))

(defun invite-user-to-all-spaces-in-space (connection user-id space-id)
  (let ((spaces (spaces-in-a-space connection space-id)))
    (mapc (lambda (space)
            (with-hash-keys (|room_id| |name|)
                space
              (format t "Inviting user to space: ~A~%" |name|)
              (handler-case 
                  (invite-member-to-room connection user-id |room_id|)
                (m-forbidden ()
                  nil))))
          spaces)))
