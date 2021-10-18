
(in-package #:lunamech-matrix-api)
;;;;this file contains a nice way to handle sync objects, by default they are
;;;;huge and unwieldy

(defparameter *type-mappings*
  '((events . event)
    (room-events . room-event)
    (state-events . state-event)
    (stripped-states . stripped-state)))

(defun get-type-mapping (expected-type)
  (cdr (assoc expected-type *type-mappings*)))


;;;minor problem here. If we wish to sync multiple times for different objects
;;;then we can't just modify the latest sync for status, we have to store each one
;;;and update it for each one. There is no problem if we only sync once for all our
;;;information but we aren't.

;;;problem solved in syncing.lisp using filter objects that maintain their own
;;;last sync 
(defun sync (connection &key filter-id timeout)
  "Gets the latest sync object from the server using CONNECTION."
  (let ((plist (when filter-id (list :|filter| filter-id))))
    (when (slot-boundp (status connection) 'latest-sync)
      (setf plist (append plist
                          (list :|since|
                                (getf (latest-sync (status connection))
                                      :|next_batch|))
                          (if timeout
                              (list :|timeout| timeout)
                              nil))))
    (auth-req (:get connection ("sync") plist resp)
      (setf (latest-sync (status connection)) resp)
      (when (slot-boundp connection 'encryption)
        (setf (server-otk (encryption connection))
              (getf (getf resp :|device_one_time_keys_count|) :|signed_curve25519|)))
      resp)))

(defun traverse-sync (sync list-of-keys)
  "The default sync that is received and then parsed from the server ends up as one big ol 
plist, so this function takes a variety of lowercase keywords ie :|imasym| and steps through
the plist using those keys."
  (loop :for key keyword :in list-of-keys
        :for sy := (getf sync key)
          :then (getf sy key)
        :always sy
        :finally (return sy)))

(defun room-timeline (sync room-id)
  (traverse-sync sync (list ':|rooms| ':|join| room-id ':|timeline| ':|events|)))

(defun room-messages (sync room-id)
  (unless (keywordp room-id)
    (setf room-id (intern room-id :keyword)))
  (let ((events (room-timeline sync room-id)))
    (remove-if-not (lambda (event)
                     (let ((type (getf event :|type|)))
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
                     (string= (getf (getf event :|content|) :|membership|) "leave"))
                   membership-events)))

(defun room-joins (membership-events)
  "Extracts all events containing a :|membership| 'join' event"
  (when membership-events
    (remove-if-not (lambda (event)
                     (string= (getf (getf event :|content|) :|membership|) "join"))
                   membership-events)))

(defun room-invite (sync)
  (traverse-sync sync (list ':|rooms| ':|invite|)))

(defun extract-events-of-type (events types)
  "Gives a list of EVENTS extracts the events that match list of TYPES"
  (check-type types list)
  (remove-if-not
   (lambda (event)
     (let ((type (getf event :|type|)))
       (some (lambda (allowed)
               (string= type allowed))
             types)))
   events))
