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

(defmethod c2mop:effective-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (cond ((eq category :send)
                       (if specialp
                           'api-send-special-effective
                           'api-send-effective))
                      (t 'api-slot-effective)))))

(defmethod c2mop:direct-slot-definition-class ((class api-call) &rest initargs)
  (destructuring-bind (&key category specialp &allow-other-keys)
      initargs
    (find-class (cond ((eq category :send)
                       (if specialp
                           'api-send-special-direct
                           'api-send-direct))
                      (t 'api-slot-direct)))))

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
    (when (first specialp)
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
    :initarg :connection)
   (result
    :accessor result))
  (:metaclass api-call)
  (:documentation "The top level class for all API call objects."))


(defmethod initialize-instance :after ((class api) &rest initargs)
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


(defmacro defapi (name (endpoint request-type) slots &rest class-options)
  `(progn (defclass ,name (api)
            ,slots
            ,@(append `((:metaclass api-call)
                        (:request-type ,request-type)
                        (:endpoint ,endpoint))
               class-options))))

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
                     (eq (category slot) :send))
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

(defmethod determine-request-fun ((key (eql :post)))
  #'dexador:post)

(defmethod determine-request-fun ((key (eql :get)))
  #'dexador:get)

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

(defmethod generate-headers ((api api))
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
    (jojo:to-json (slot-value api special-slot))))

(defmethod generate-body%normal ((api api))
  (to-json api))

(defmethod generate-body ((api api))
  (if (specialp api)
      (generate-body%special api)
      (generate-body%normal api)))

(defmethod call-api ((api api))
  (symbol-macrolet ((string-constructor (string-constructor api))
                    (request-type (request-type api))
                    (connection (connection api)))
    (let* ((end (apply string-constructor (values-for-required api)))
           (fun (determine-request-fun request-type))
           (api-string (api api)))
      (with-accessors ((url url)
                       (auth auth))
          connection
        (let ((headers (generate-headers api))
              (content (to-json api))
              (url (concatenate 'string url api-string end)))
          (let ((res
                  (execute-api-call api fun url 
                                    `(:headers ,headers :content ,content
                                      :use-connection-pool nil))))
            (setf (result api) (jojo:parse res))
            api))))))

(defmethod execute-api-call ((api api) fun url args-plist)
  (with-captured-dex-error
    (apply fun url args-plist)))
