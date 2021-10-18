(in-package #:lunamech-matrix-api/v2)

(defun serialize-connection (connection
                             &optional (slots '(url username logged-in-p auth
                                                device-id txn
                                                api filters status user-id)))
  (flet ((when-boundp (slot)
           (let ((key (intern (string-upcase slot) :keyword)))
             (if (slot-boundp connection slot)
                 (list key (serialize-object (slot-value connection slot)))
                 (list key :no-val)))))        
    (reduce #'append
            (mapcar #'when-boundp slots))))

(defmethod serialize-object (obj)
  obj)

(defmethod serialize-object ((obj auth))
  (list :token
        (if (slot-boundp obj 'token)
            (slot-value obj 'token)
            :no-val)))

(defmethod serialize-object ((obj list))
  (mapcar #'serialize-object obj))

(defmethod serialize-object ((obj filter))
  (with-slots (key id)
      obj
    (list :key key :id id
          :last-sync-string (if (slot-boundp obj 'last-sync-string)
                                (slot-value obj 'last-sync-string)
                                :no-val)
          :next-sync-string (if (slot-boundp obj 'next-sync-string)
                                (slot-value obj 'next-sync-string)
                                :no-val))))

(defmethod serialize-object ((obj status))
  (list :next-batch
        (if (slot-boundp obj 'next-batch)
            (slot-value obj 'next-batch)
            :no-val)
        :latest-sync :no-val))

(defmethod restore-from-key ((key (eql :status)) list)
  (apply 'make-instance 'status list))

(defmethod restore-from-key ((key (eql :auth)) list)
  (apply 'make-instance 'auth list))

(defmethod restore-from-key ((key (eql :filters)) list)
  (mapcar (lambda (lst)
            (restore-from-key :filter lst))
          list))

(defmethod restore-from-key ((key (eql :filter)) list)
  (apply 'make-instance 'filter list))

(defmethod restore-from-key :around (key list)
  (let ((res (call-next-method)))
    (when (typep res 'c2mop:standard-object)
      (let ((slots (c2mop:class-direct-slots (class-of res))))
        (mapc (lambda (slot)
                (let* ((slot-name (c2mop:slot-definition-name slot))
                       (val (slot-value res slot-name)))
                  (when (eq val :no-val)
                    (slot-makunbound res slot-name))))
              slots)))
    res))

(defmethod restore-from-key (key list)
  list)

(defun restore-connection (list &optional
                                  (slots '(url username logged-in-p auth
                                           device-id status txn
                                           api filters status user-id)))
  (let ((connection (make-instance 'connection)))
    (dolist (slot slots connection)
      (let* ((slot-key (intern (string-upcase (symbol-name slot)) :keyword))
             (val (getf list slot-key)))
        (setf (slot-value connection slot)
              (restore-from-key slot-key val))))))

