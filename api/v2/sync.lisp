
(in-package #:lunamech-matrix-api/v2)

(defun sync (connection &rest keys &key &allow-other-keys)
  "Gets the latest sync object from the server using CONNECTION."
  (with-accessors ((status status))                   
      connection
    (let ((call (apply #'make-instance 'sync
                       (append (list :connection connection)
                               keys))))
      (when (slot-boundp status 'next-batch)
        (setf (since call) (next-batch status)))
      (let ((resp (call-api call)))
        (when (slot-boundp connection 'encryption)
          (setf (server-otk (encryption connection))
                (gethash "signed_curve25519"
                         (gethash "device_one_time_keys_count" resp))))
        (setf (latest-sync status) resp
              (next-batch status) (gethash "next_batch" resp))
        resp))))

(defun dry-sync (connection &rest keys &key &allow-other-keys)
  (when (slot-boundp (status connection) 'next-batch)
    (slot-makunbound (status connection) 'next-batch))
  (apply #'sync connection keys))

(defun key-sync (connection filter-key &rest keys &key &allow-other-keys)
  (let ((filter (find filter-key (filters connection) :key #'key :test #'eq)))
    (or filter (error "No key found."))
    (apply #'sync connection (append (list :filter (id filter))
                                     keys))))

(defun traverse-sync (sync list-of-keys)
  "The default sync that is received and then parsed from the server ends up as one big ol 
plist, so this function takes a variety of lowercase keywords ie :|imasym| and steps through
the plist using those keys."
  (loop :for key string :in list-of-keys
        :for sy := (gethash key sync)
          :then (gethash key sy)
        :always sy
        :finally (return sy)))

(defun room-timeline (sync room-id)
  (traverse-sync sync (list "rooms" "join" room-id "timeline" "events")))

(defun room-messages (sync room-id)
  (unless (keywordp room-id)
    (setf room-id (intern room-id :keyword)))
  (let ((events (room-timeline sync room-id)))
    (remove-if-not (lambda (event)
                     (let ((type (gethash "type" event)))
                       (or (string= type "m.room.message")
                           (string= type "m.room.encrypted"))))
                   events)))

(defun membership-events (sync room-id)
  (unless (keywordp room-id)
    (setf room-id (intern room-id :keyword)))
  (let* ((events (room-timeline sync room-id))
         (members (extract-events-of-type events '("m.room.member"))))
    members))

(defun room-leaves (membership-events)
  "Extracts all events containing a :|membership| 'leave' event"
  (when membership-events
    (remove-if-not (lambda (event)
                     (string= (gethash "membership" (gethash "content" event))
                              "leave"))
                   membership-events)))

(defun room-joins (membership-events)
  "Extracts all events containing a :|membership| 'join' event"
  (when membership-events
    (remove-if-not (lambda (event)
                     (string= (gethash "membership" (gethash "content" event))
                              "join"))
                   membership-events)))

(defun room-invite (sync)
  (traverse-sync sync (list "rooms" "invite")))

(defun extract-events-of-type (events types)
  "Gives a list of EVENTS extracts the events that match list of TYPES"
  (check-type types list)
  (remove-if-not
   (lambda (event)
     (let ((type (gethash "type" event)))
       (some (lambda (allowed)
               (string= type allowed))
             types)))
   events))

