(in-package #:lunamech-matrix-api/v2)
#||
This file contains the code for a metaobject protocol used for 
specifying the matrix API calls.
Right now it must simply replace the (auth-req ..) functionality
as the interface cannot be changed, although all newly implemented API calls can 
follow a different scheme.
||#

(defun url-e (url)
  (quri:url-encode url))

(defclass api ()
  ((connection
    :reader connection
    :initarg :connection
    :type connection
    :category :receive
    :requiredp nil)
   (result
    :accessor result
    :category :receive
    :category nil
    :requiredp nil))
  (:metaclass api-call)
  (:documentation "The top level class for all API call objects."))

(defmethod initialize-instance :after ((class api) &rest initargs)
  (handler-case (connection class)
    (condition (c)
      (error 'connection-unbound :message "Connection is unbound.")))
  (when (contains-txn-p class)
    (setf (slot-value class 'txn)
          (txn (connection class)))))

;;;code for generating the string constructor function

(defun cleaned-slot-name (slot)
  (symbol-name (c2mop:slot-definition-name slot)))

(defun %find-encoders-for-syms (sym-string-list effective direct)
  "Maps over SYM-STRING-LIST which is a mix of symbols and strings, if its a string
returns `(list ,string) if it is a symbol then looks for the slot with the name 
,entry and the returns `(cons ,entry ,(encoder slot)), this final accumulated list is 
returned."
  (remove #'null
          (mapcar (lambda (entry)
                    (if (stringp entry)
                        `,entry
                        (let ((effective (find entry effective :key #'cleaned-slot-name
                                                               :test #'string-equal))
                              (direct (find entry direct :key #'cleaned-slot-name
                                                         :test #'string-equal)))
                          (or (and effective direct)
                              (error "both slot and direct need to be bound."))
                          (list :effective effective :direct direct))))
                  sym-string-list)))


(defun %upcase-and-intern-starting-with (start list)
  "If a string in LIST starts with START then interns and upcases it with START removed.
START should be a string of len 1."
  (mapcar (lambda (string)
            (if (str:starts-with-p start string)
                (intern (string-upcase (subseq string 1)))
                string))
          list))

(defun grab-slot-value (class object effective)
  (handler-case
      (c2mop:slot-value-using-class class object effective)
    (unbound-slot ()
      (error 'missing-required-data
             :slot effective))))

(defun compose-string-into-lambda (class string)
  "Takes a string like 'foo/:bar/quux and returns a function with N arguments where 
N is the number of words prefixed with :, so in the example it would be a function with 
one argument bar. When executed with this argument :bar would be replaced and a new 
string returned. Checks the slot related to the : argument to see if there is an associated 
encoder, if there is then it encodes that argument with that single argument function. 
Also checks if the argument is optional, if it is optional then it is added as an 
&optional argument to the returned function with a default argument of nil, this is 
removed if no value is added."
  (let* ((direct-slots (remove-if-not #'in-url-p (c2mop:class-direct-slots class)))
         (required-effective-slots (intersection (c2mop:class-slots class)
                                                 direct-slots 
                                                 :key #'c2mop:slot-definition-name
                                                 :test #'string-equal)))
    (if direct-slots 
        (let* ((split (str:split #\/ string))                 
               (replaced-with-syms (%upcase-and-intern-starting-with ":" split))
               (syms-assoc-encoders (%find-encoders-for-syms replaced-with-syms
                                                             required-effective-slots
                                                             direct-slots)))
          (when (find ":txn" split :test #'string-equal)
            (setf (contains-txn-p class) t))
          (compile nil `(lambda (api)
                          (let ((strings
                                  (mapcar
                                   (lambda (e)
                                     (etypecase e
                                       (string e)
                                       (list
                                        (destructuring-bind (&key effective direct)
                                            e 
                                          (let ((val (grab-slot-value ,class api effective))
                                                (encoder? (encoder direct)))
                                            (if encoder?
                                                (etypecase val
                                                  (string (funcall encoder? val))
                                                  (number val))
                                                val))))))
                                   ',syms-assoc-encoders)))
                            (format nil "~{~A~^/~}" strings)))))
        (lambda (api)
          (declare (ignore api))
          string))))

(defmacro defapi (name (endpoint) docstring slots
                  &rest class-options)
  `(progn (let ((class (defclass ,name (api)
                         ,slots
                         ,@(append `((:metaclass api-call)                        
                                     (:endpoint ,endpoint)
                                     (:documentation ,docstring))
                            class-options))))
            (c2mop:ensure-finalized class)
            (with-slots (string-constructor endpoint)
                class 
              (setf string-constructor
                    (compose-string-into-lambda class (in-list endpoint)))))))

(defmacro defapi%post (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint) ,docstring ,slots
           ,@(append class-options
                     '((:request-fun :post)))))

(defmacro defapi%get (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint) ,docstring ,slots
           ,@(append class-options
                     '((:request-fun :get)))))

(defmacro defapi%put (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint) ,docstring ,slots
           ,@(append class-options
                     '((:request-fun :put)))))

(defmacro defapi%delete (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint) ,docstring ,slots
           ,@(append class-options
                     '((:request-fun :delete)))))

(defmethod validate-slot-for-sending ((api api) slot)
  (with-accessors ((requiredp requiredp))
      slot
    (when (and requiredp (not (slot-boundp api (c2mop:slot-definition-name slot))))
      (error 'missing-required-data
             :slot slot
             :message "Trying to send a request but missing data from required slot."))))

(defmethod slots-to-send ((api api))
  (let ((slots (c2mop:class-direct-slots (class-of api))))
    (remove-if-not (lambda (slot)
                     (and (eq (category slot) :send)
                          (not (query-param-p slot))
                          (not (in-url-p slot))))
                   slots)))

(defmethod validate-slots-for-sending ((api api) slots-to-send)
  (mapc (lambda (slot)
          (validate-slot-for-sending api slot))
        slots-to-send))

(defmethod remove-unbound-slots ((api api) slots-to-send)
  (remove-if-not (lambda (slot)
                   (slot-boundp api (c2mop:slot-definition-name slot)))
                 slots-to-send))

(defmethod slots-to-send-and-validated ((api api))
  (remove-unbound-slots api (validate-slots-for-sending api (slots-to-send api))))

(defun determine-slot-name (api-slot)
  "If 'name->json is bound within API-SLOT then uses that for the name rather than 
converting the #'c2mop:slot-definition-name to snake-case."
  (or (and (slot-boundp api-slot 'name->json)
           (slot-value api-slot 'name->json))
      (str:snake-case (string (c2mop:slot-definition-name api-slot)))))

(defun slot->json (api hash slot)
  "Converts a slot from within an instance of API into a value that
will be converted to JSON. Uses HASH for this."
  (let ((name (determine-slot-name slot)))
    (setf (gethash name hash) 
          (slot-value api (c2mop:slot-definition-name slot)))))

(defun to-json (api)
  "Converts an instance of API into a JSON string. Currently uses SHASHT."
  (let* ((slots (slots-to-send-and-validated api))
         (hash (make-hash-table :test #'equal)))
    (mapc (lambda (s) (slot->json api hash s)) slots)
    (shasht:write-json hash nil)))

(defun in-list (e)
  (if (listp e)
      (first e)
      e))

(defmethod endpoint ((api api))
  (in-list (endpoint (class-of api))))

(defmethod contains-txn-p ((api api))
  (in-list (contains-txn-p (class-of api))))

(defmethod string-constructor ((api api))
  (string-constructor (class-of api)))

(defmethod do-not-decode-p ((api api))
  (in-list (do-not-decode-p (class-of api))))

(defmethod request-type ((api api))
  (in-list (request-type (class-of api))))

(defmethod rate-limited-p ((api api))
  (in-list (rate-limited-p (class-of api))))

(defmethod api ((api api))
  (in-list (api (class-of api))))

(defmethod content-type ((api api))
  (in-list (content-type (class-of api))))

(defmethod requires-auth-p ((api api))
  (in-list (requires-auth-p (class-of api))))

(defmethod request-fun ((api api))
  (in-list (request-fun (class-of api))))

(defmethod specialp ((api api))
  (in-list (specialp (class-of api))))

(defmethod required-slots ((api api))
  (required-slots (class-of api)))

(defmethod special-slot ((api api))
  (special-slot (class-of api)))

(defmethod values-for-required ((api api))
  (let ((req (required-slots api)))
    (loop :for name :in req
          :when (slot-boundp api name)
            :collect (slot-value api name))))

(defmethod generate-authorization ((api api))
  (with-accessors ((connection connection))
      api
    (let* ((auth (auth connection))
           (token (token auth)))
      (format nil "Bearer ~A" token))))

(defmethod generate-content-type ((api api))
  (let ((content-type (content-type api)))
    (if (stringp content-type)
        content-type
        (slot-value api content-type))))

(defmethod generate-authorization-headers ((api api))
  (cons (cons "Content-Type" (generate-content-type api))
        (if (requires-auth-p api)
            (cons (cons "Authorization" (generate-authorization api))
                  nil))))

(defmethod generate-body%special ((api api))
  (with-accessors ((special-slot special-slot))
      api 
    (unless (slot-boundp api special-slot)
      (error 'special-slot-is-not-bound 
             :message "You have declared a slot special but it is not bound..."))
    (if (eql special-slot 'bytes)
        (slot-value api special-slot)
        (shasht:write-json (slot-value api special-slot) nil))))

(defmethod generate-body%normal ((api api))
  (to-json api))

(defmethod generate-body ((api api))
  (if (specialp api)
      (generate-body%special api)
      (generate-body%normal api)))

(defgeneric generate-header-list (api fun content)
  (:documentation "Generates the header list for an api call"))

(defmethod generate-header-list ((api api) fun content)
  (let ((auth (generate-authorization-headers api)))
    `(:headers ,auth :content ,content)))

(defmethod generate-header-list ((api api) (fun (eql 'dexador:get)) content)
  (let ((auth (generate-authorization-headers api)))
    `(:headers ,auth)))

(defmethod generate-header-list ((api api) (fun (eql 'dexador:delete)) content)
  (let ((auth (generate-authorization-headers api)))
    `(:headers ,auth)))

(defmethod all-query-param-slots (slots)
  (remove-if-not #'query-param-p slots))

(defmethod bound-query-param-slots ((api api))
  (let ((slots (all-query-param-slots (c2mop:class-direct-slots (class-of api)))))
    (remove-if-not (lambda (slot)
                     (let ((boundp (slot-boundp api (c2mop:slot-definition-name slot))))
                       (when boundp 
                         (slot-value api (c2mop:slot-definition-name slot)))))
                   slots)))

(defmethod query-param-slot->string ((api api) slot)
  (with-accessors ((name->json name->json))
      slot
    (let ((name (c2mop:slot-definition-name slot)))
      (format nil "~A=~A" (if name->json name->json (str:snake-case (correct-encode name)))
              (correct-encode (slot-value api name))))))

(defmethod correct-encode (value)
  "Correctly encode VALUE based on whether it is a string or a symbol."
  (typecase value
    (string (quri:url-encode value))
    (symbol (quri:url-encode (symbol-name value)))
    (otherwise value)))

(defmethod query-param-slots->string ((api api) slots)
  (let ((strings (mapcar (lambda (slot) (query-param-slot->string api slot)) slots)))
    (format nil "?~{~A~^&~}" strings)))

(defmethod generate-url ((api api))
  (handler-case 
      (let ((end (funcall (string-constructor api) api))
            (query-slots (bound-query-param-slots api)))
        (concatenate 'string (url (connection api)) (api api) end
                     (if query-slots
                         (query-param-slots->string api query-slots)
                         "")))
    (missing-required-data (c)
      (format nil "MISSING-REQUIRED-DATA: ~A" (c2mop:slot-definition-name (slot c))))))

(defmethod call-api ((api api))
  (symbol-macrolet ((fun (request-fun api))
                    (connection (connection api)))
    (with-accessors ((url url)
                     (auth auth)
                     (logged-in-p logged-in-p))
        connection
      (when (requires-auth-p api)
        (or logged-in-p (error "You have to be logged in to use that command.")))
      (when (contains-txn-p api)
        (with-locked-connection ((connection api))
          (setf (slot-value api 'txn)
                (incf (txn (connection api))))))
      (let ((url (generate-url api))
            (header-list (generate-header-list api fun (generate-body api))))
        (setf (result api)
              (if (do-not-decode-p api)
                  (execute-api-call api fun url header-list)
                  (jojo:parse (execute-api-call api fun url header-list)
                              :as :hash-table)))
        (values (result api) api)))))

(defmethod slots-still-missing ((api api))
  (let* ((slots (c2mop:class-direct-slots (class-of api)))
         (required (remove-if-not #'requiredp slots)))
    (remove-if (lambda (slot)
                 (slot-boundp api (c2mop:slot-definition-name slot)))
               required)))

(defmethod execute-api-call ((api api) fun url args-plist)
  (with-captured-dex-error
    (apply #'dex:request url (append (list :method fun) args-plist))))

(defmethod print-object ((obj api) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%~A ~A~%Content-Type: ~A~%JSON: ~A~%MISSING: ~{~A~^, ~}"
            (request-fun obj)
            (generate-url obj)
            (generate-content-type obj)
            (let ((body (generate-body obj)))
              (if (> (length body) 300)
                  (subseq body 0 300)
                  body))
            (let ((missing (slots-still-missing obj)))
              (if missing
                  (mapcar #'c2mop:slot-definition-name missing)
                  (list 'NONE))))))

