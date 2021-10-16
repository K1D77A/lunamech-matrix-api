(in-package #:lunamech-matrix-api)

(defun stringsp (seq)
  (every #'stringp seq))

(deftype strings () `(satisfies stringsp))

(defparameter *test*
  '(("room"
     ("state" ("types" ("m.room.message")))
     ("timeline" ("limit" 100) ("types" ("m.room.message")))
     ("ephemeral" ("limit" 100) ("types" ("m.receipt" "m.typing"))))
    ("presence"
     ("types" ("m.presence"))
     ("not_senders" ("@alice:example.com")))
    ("event_format" "client")
    ("event_fields" ("type" "content" "sender"))))

(defclass filter-object ()
  ())

(defclass filter-request (filter-object)
  ((userid :type string)
   (event_fields :type strings)
   (event_format :type string)
   (presence :type event-filter)
   (account_data :type event-filter)
   (room :type room-filter)))

(defclass event-filter (filter-object)
  ((limit :type integer)
   (not_senders :type strings)
   (not_types :type strings)
   (senders :type strings)
   (types :type strings)))

(defclass room-filter (filter-object)
  ((not_rooms :type strings)
   (rooms :type strings)
   (ephemeral :type room-event-filter)
   (include_leave :type boolean)
   (state :type room-event-filter)
   (timeline :type room-event-filter)
   (account_data :type room-event-filter)))

(defclass room-event-filter (filter-object)
  ((limit :type integer)
   (not_senders :type strings)
   (not_types :type strings)
   (senders :type strings)
   (types :type strings)
   (rooms :type strings)
   (contains_url :type boolean)))

(declaim (inline to-sym))
(defun to-sym (string)
  (intern (string-upcase string) :matrix-moonbot))

(defun filter-list-entry->object (parent-object entry)
  "Attempts to convert a list like (first *test*) into its applicable objects. 
Each key is checked to be a slot within PARENT-OBJECT, and the type is then checked,
if the type is another object that object is created, the appropriate slot is set 
to that slot and then the function is called again with the rest of that list 
and the new parent object. if the slot type is not a new object then the slot is 
set to rest of that list."
  (if (and (listp (second entry)) (not (stringsp (second entry))))
      (let ((object? (instance-from-string parent-object (first entry))))
        (setf (slot-value parent-object (to-sym (first entry))) object?)
        (loop :for list :in (rest entry)
              :do (filter-list-entry->object object? list)))
      (setf (slot-value parent-object (to-sym (first entry))) (second entry)))
  parent-object)

(defun instance-from-string (parent string)
  "Takes in an object called PARENT, and a STRING, attempts to find the slot 
within parent whose symbol would be equal to that of STRING, then checks the type,
if the type is a new object then creates the new object and returns, if not returns
nil."
  (let* ((slots (c2mop:class-slots (class-of parent)))
         (slot (find string slots
                     :key (lambda (y)
                            (string-downcase (c2mop:slot-definition-name y)))
                     :test #'string=)))
    (when slot
      (handler-case (make-instance (c2mop:slot-definition-type slot))
        (condition ()
          nil)))))

(defun filter-list->filter-request (list)
  "Attempts to convert a LIST like *test* into a single filter-request object. 
See documentation for FILTER-LIST-ENTRY->OBJECT for more details."
  (let ((request (make-instance 'filter-request)))
    (mapc (lambda (object)
            (filter-list-entry->object request object))
          list)
    request))

(defmethod jojo:%to-json ((obj filter-object))
  "Attempts to convert any instance of subclass FILTER-OBJECT to its appropriate
json, because there are loads of potential slots within FILTER-OBJECT, this uses
the MOP to check if a slot is bound and will only convert bound slots."
  (let ((slots (c2mop:class-slots (class-of obj))))
    (jojo:with-object
      (loop :for slot :in slots
            :when (slot-boundp obj (c2mop:slot-definition-name slot))
              :do (let* ((slot-name (c2mop:slot-definition-name slot))
                         (as-string (string-downcase (symbol-name slot-name))))
                    (jojo:write-key-value as-string (slot-value obj slot-name)))))))




