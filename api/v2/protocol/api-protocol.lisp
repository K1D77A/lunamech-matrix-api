(in-package #:lunamech-matrix-api/v2)
#||
This file contains the code for a metaobject protocol used for 
specifying the matrix API calls.
Right now it must simply replace the (auth-req ..) functionality
as the interface cannot be changed, although all newly implemented API calls can 
follow a different scheme.
||#


;;;;this file contains a MOP for webhooks



(defclass api-slot-direct (api-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-slot-effective (api-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-send-direct (api-send-slot c2mop:standard-direct-slot-definition)
  ())

(defclass api-send-effective (api-send-slot c2mop:standard-effective-slot-definition)
  ())

(defclass api-send-special-direct (api-send-slot-special
                                   c2mop:standard-direct-slot-definition)
  ())

(defclass api-send-special-effective (api-send-slot-special
                                      c2mop:standard-effective-slot-definition)
  ())


(defmethod c2mop:validate-superclass ((class api-call)
                                      (metaclass c2mop:funcallable-standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-send-slot)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-send-slot-special)
                                      (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class api-slot)
                                      (metaclass standard-class))
  t)

(defmethod initialize-instance :after ((class api-slot) &rest initargs)
  (validate-slot class))

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (if (and category (not (eq category :send)))
                    'api-slot-effective
                    (if specialp
                        'api-send-special-effective
                        'api-send-effective)))))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (if (and category (not (eq category :send)))
                    'api-slot-direct
                    (if specialp
                        'api-send-special-direct
                        'api-send-direct)))))

(defmethod c2mop:compute-effective-slot-definition ((class api-call) name dslots)
  (call-next-method))

(defmethod c2mop:compute-slots ((class api-call))
  (with-slots (string-constructor endpoint required-slots special-slot)
      class
    (multiple-value-bind (fun slots)
        (compose-string-into-lambda class (in-list endpoint))
      (setf string-constructor fun
            required-slots slots
            special-slot (find-special-slot class))
      (call-next-method))))

(defmethod find-special-slot ((class api-call))
  (with-slots (special-slot specialp)
      class
    (when (in-list (specialp class))
      (let* ((slots (c2mop:class-direct-slots class))
             (slot (first (remove-if-not  (lambda (slot) (typep slot 'api-send-special-direct))
                                          slots))))
        (if slot
            (c2mop:slot-definition-name slot)
            (error 'set-special
                   :message "Class is set to special but you have no slots that are declared special"))))))



(defun url-e (url)
  (do-urlencode:urlencode url))


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

(defclass %post (api)
  ()
  (:metaclass api-call))

(defclass %get (api)
  ()
  (:metaclass api-call))

(defclass %delete (api)
  ()
  (:metaclass api-call))

(defclass %put (api)
  ()
  (:metaclass api-call))

(defmethod initialize-instance :after ((class api) &rest initargs)
  (handler-case (connection class)
    (condition (c)
      (error 'connection-unbound :message "Connection is unbound.")))
  (with-slots (string-constructor)
      (class-of class)
    (c2mop:set-funcallable-instance-function class (lambda () (funcall #'call-api class)))))
;;(call-next-method)))

;;;code for generating the string constructor function
(defun %find-encoders-for-syms (sym-string-list slots)
  "Maps over SYM-STRING-LIST which is a mix of symbols and strings, if its a string
returns `(list ,string) if it is a symbol then looks for the slot with the name 
,entry and the returns `(cons ,entry ,(encoder slot)), this final accumulated list is 
returned."
  (mapcar (lambda (entry)
            (if (stringp entry)
                `(list ,entry)
                (let ((slot (find entry slots :key #'c2mop:slot-definition-name)))
                  `(cons ,entry ,(encoder slot)))))
          sym-string-list))

(defun %upcase-and-intern-starting-with (start list)
  "If a string in LIST starts with START then interns and upcases it with START removed.
START should be a string of len 1."
  (mapcar (lambda (string)
            (if (str:starts-with-p start string)
                (intern (string-upcase (subseq string 1)))
                string))
          list))

(defun %filter-syms-not-assoc-with-slots (list-of-syms slots)
  "Removes symbols from LIST-OF-SYMS that do not have associated names in SLOTS."
  (remove-if #'null
             (mapcar (lambda (sym) (find sym slots :key #'c2mop:slot-definition-name))
                     list-of-syms)))

(defun %construct-lambda-list (required-vars optional-vars)
  "Constructs a lambda list from the two lists of symbols REQUIRED-VARS and OPTIONAL-VARS."
  (append required-vars (if optional-vars '(&optional) nil)
          (mapcar (lambda (name)
                    (list name nil))
                  optional-vars)))

(defun compose-string-into-lambda (class string)
  "Takes a string like 'foo/:bar/quux and returns a function with N arguments where 
N is the number of words prefixed with :, so in the example it would be a function with 
one argument bar. When executed with this argument :bar would be replaced and a new 
string returned. Checks the slot related to the : argument to see if there is an associated 
encoder, if there is then it encodes that argument with that single argument function. 
Also checks if the argument is optional, if it is optional then it is added as an 
&optional argument to the returned function with a default argument of nil, this is 
removed if no value is added."
  (flet ((slot-name (slot) (c2mop:slot-definition-name slot)))
    (let ((class-slots (c2mop:class-direct-slots class)))
      (if (some #'in-url-p class-slots)
          (let* ((split (str:split #\/ string))
                 (replaced-with-syms (%upcase-and-intern-starting-with ":" split))
                 (syms-assoc-encoders (%find-encoders-for-syms replaced-with-syms
                                                               class-slots))
                 (lambda-list (remove-if-not #'symbolp replaced-with-syms))
                 (filtered-slots (%filter-syms-not-assoc-with-slots lambda-list class-slots))
                 (optional-slots (remove-if #'requiredp filtered-slots))
                 (required-slots (remove-if-not #'requiredp filtered-slots))
                 (o-names (mapcar #'slot-name optional-slots))
                 (r-names (mapcar #'slot-name required-slots))
                 (final-lambda-list (%construct-lambda-list r-names o-names)))       
            (values     
             (compile nil
                      `(lambda ,final-lambda-list
                         (format nil "~{~A~^/~}"
                                 (mapcar (lambda (list)
                                           (let ((str-or-sym (car list))
                                                 (encoder? (cdr list)))
                                             (if encoder?
                                                 (funcall encoder? str-or-sym)
                                                 str-or-sym)))
                                         (remove-if #'null (list ,@syms-assoc-encoders)
                                                    :key #'car)))))
             lambda-list))
          (lambda ()
            string)))))


(defmacro defapi (name (endpoint request-class metaclass) docstring slots &rest class-options)
  `(progn (defclass ,name (,request-class)
            ,slots
            ,@(append `((:metaclass ,metaclass)                        
                        (:endpoint ,endpoint)
                        (:documentation ,docstring))
               class-options))))

(defmacro defapi%post (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint %post api-call%post) ,docstring ,slots ,@class-options))

(defmacro defapi%get (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint %get api-call%get)  ,docstring ,slots ,@class-options))

(defmacro defapi%put (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint %put api-call%put) ,docstring ,slots ,@class-options))

(defmacro defapi%delete (name (endpoint) docstring slots &rest class-options)
  `(defapi ,name (,endpoint %delete api-call%delete) ,docstring ,slots ,@class-options))

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
  (remove-if-not (lambda (slot) (slot-boundp api (c2mop:slot-definition-name slot)))
                 slots-to-send))

(defmethod slots-to-send-and-validated ((api api))
  (remove-unbound-slots api (validate-slots-for-sending api (slots-to-send api))))

(defmethod slot->json ((api api) slot)
  (let ((name (c2mop:slot-definition-name slot)))
    (cl-json:encode-object-member 
     (or (and (slot-boundp slot 'name->json)(slot-value slot 'name->json))
         (str:snake-case (string name)))
     (slot-value api name))))

(defmethod to-json ((api api))
  (let* ((slots (slots-to-send-and-validated api))
         (stream (make-string-output-stream))
         (cl-json:*json-output* stream))
    (cl-json:with-object ()
      (mapc (lambda (s) (slot->json api s)) slots))
    (get-output-stream-string stream)))

(defun in-list (e)
  (if (listp e)
      (first e)
      e))

(defmethod string-constructor ((api api))
  (string-constructor (class-of api)))

(defmethod request-type ((api api))
  (in-list (request-type (class-of api))))

(defmethod rate-limited-p ((api api))
  (in-list (rate-limited-p (class-of api))))

(defmethod api ((api api))
  (in-list (api (class-of api))))

(defmethod content-type ((api api))
  (content-type (class-of api)))

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
  (content-type api))

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
    (jojo:to-json (if (slot-boundp api special-slot)
                      (slot-value api special-slot)
                      "Special slot has no value"))))

(defmethod generate-body%normal ((api api))
  (to-json api))

(defmethod generate-body ((api api))
  (if (specialp api)
      (generate-body%special api)
      (generate-body%normal api)))

(defmethod generate-header-list ((api api) content)
  (let ((auth (generate-authorization-headers api)))
    `(:headers ,auth :content content :use-connection-pool nil)))

(defmethod generate-header-list ((api %get) content)
  (declare (ignore content))
  (let ((auth (generate-authorization-headers api)))
    `(:headers ,auth :use-connection-pool nil)))

(defmethod all-query-param-slots (slots)
  (remove-if-not #'query-param-p slots))

(defmethod bound-query-param-slots ((api api))
  (let ((slots (all-query-param-slots (c2mop:class-direct-slots (class-of api)))))
    (remove-if-not (lambda (slot) (slot-boundp api (c2mop:slot-definition-name slot))) slots)))

(defmethod query-param-slot->string ((api api) slot)
  (with-accessors ((name->json name->json))
      slot
    (let ((name  (c2mop:slot-definition-name slot)))
      (format nil "~A=~A" (if name->json name->json (str:snake-case (correct-encode name)))
              (correct-encode (slot-value api name))))))

(defmethod correct-encode (value)
  (typecase value
    (string (url-e value))
    (symbol (url-e (symbol-name value)))
    (otherwise value)))

(defmethod query-param-slots->string ((api api) slots)
  (let ((strings (mapcar (lambda (slot) (query-param-slot->string api slot)) slots)))
    (format nil "?~{~A~^&~}" strings)))

(defmethod generate-url ((api api))
  (let ((end (apply (string-constructor api) (values-for-required api)))
        (query-slots (bound-query-param-slots api)))
    (concatenate 'string (url (connection api)) (api api) end
                 (if query-slots
                     (query-param-slots->string api query-slots)
                     ""))))

(defmethod call-api ((api api))
  (symbol-macrolet ((fun (request-fun api))
                    (connection (connection api)))
    (with-accessors ((url url)
                     (auth auth))
        connection
      (let ((url (generate-url api))
            (header-list (generate-header-list api (to-json api))))
        (setf (result api) (jojo:parse (execute-api-call api fun url header-list)))
        (values (result api) api)))))

(defmacro with-captured-dex-error (&body body)
  "Catches any conditions signalled by dex and converts the response into a 
special condition defined in src/classes.lisp and signals."
  (alexandria:with-gensyms (condition)
    `(labels ((try-again-restart (fun)
                (restart-case
                    (funcall fun)
                  (try-again ()
                    :report "Try again?"
                    (sleep 3)
                    (try-again-restart fun)))))
       (let ((fun
               (lambda ()
                 (handler-case
                     (locally (bt:with-timeout (30)
                                ,@body))
                   (sb-ext:timeout (,condition)
                     (error 'api-timeout :api-timeout-message "Connection broken"
                                         :api-timeout-condition ,condition))
                   (usocket:socket-condition (,condition)
                     (error 'api-no-connection :api-timeout-message "No network"
                                               :api-timeout-condition ,condition))
                   (condition (,condition);;total catchall oh well
                     (handler-case 
                         (signal-condition-from-response
                          (jojo:parse (dexador.error:response-body ,condition)))
                       (jojo:<jonathan-error> (,condition)
                         (error 'api-no-connection
                                ;;if the server is down then this will end up
                                ;;returning html not json
                                :api-timeout-message "Server probably down"
                                :api-timeout-condition ,condition))))))))
         (try-again-restart fun)))))


(defmethod slots-still-missing ((api api))
  (let* ((slots (c2mop:class-direct-slots (class-of api)))
         (required (remove-if-not #'requiredp slots)))
    (remove-if (lambda (slot)
                 (slot-boundp api (c2mop:slot-definition-name slot)))
               required)))

(defmethod execute-api-call ((api api) fun url args-plist)
  (with-captured-dex-error
    (apply fun url args-plist)))

(defmethod print-object ((obj api) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%~A ~A~%JSON: ~A~%MISSING: ~{~A~^, ~}"
            (request-fun obj)
            (generate-url obj)
            (generate-body obj)
            (let ((missing (slots-still-missing obj)))
              (if missing
                  (mapcar #'c2mop:slot-definition-name missing)
                  (list 'NONE))))))

