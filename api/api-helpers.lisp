(in-package #:lunamech-matrix-api)

(defparameter *request-type-hash* (make-hash-table))

(defparameter +content-type+ "application/json; charset=utf-8");;I will back to const

(defun gen-headers (connection &optional (content-type +content-type+))
  "Generates a header list for use with dex"
  `(("Content-Type" . ,content-type)
    ("Authorization" . ,(format nil "Bearer ~A" (token (auth connection))))))

(defun gen-url (connection &optional (prefix nil))
  "Generates a base uri for CONNECTION. if MEDIA is non nil then uses 
/_matrix/media/r0/ in place of (api CONNECTION). Returns a list whose first element
is the url."
  (list (concatenate 'string (url connection) (key->url connection prefix))))

(defgeneric key->url (connection key)
  (:documentation "converts a keyword like :MSC2946 into a URL. If key is a string 
evaluates to that string, if it is :DEFAULT evaluates to (api connection), if it is 
anything else evaluates to (api connection)"))

(defmethod key->url (connection (key (eql :MSC2946)))
  "/_matrix/client/unstable/org.matrix.msc2946/")

(defmethod key->url (connection (key string))
  key)

(defmethod key->url (connection (key (eql :DEFAULT)))
  (api connection))

(defmethod key->url (connection key)
  (api connection))

(defmethod key->url (connection (key (eql :MEDIA)))
  "/_matrix/media/r0/")

(defun gen-admin-url (connection)
  (list (url connection)))


(defun plist-to-get-params (plist)
  "Loops over PLIST and creates a single string from the keys and vals. 
Keys are displayed with the following format string ~A= and vals ~A&. Vals are 
also url encoded"
  (apply #'concatenate 'string
         (loop :for str :in plist
               :for n :from 0 :to (length plist)
               :collect 
               (if (evenp n)
                   (format nil "~A=" str)
                   (format nil "~A&" (url-e (if (numberp str)
                                                (format nil "~D" str)
                                                str)))))))
;;;post requests
(psetf dex:*default-read-timeout* 3
       dex:*default-connect-timeout* 3)

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
                   ((or usocket:socket-condition
                     usocket:ns-condition
                     usocket:socket-error
                     usocket:timeout-error
                     usocket:unknown-error
                     usocket:ns-error) (,condition)
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

;;;post requests. theres a few as there are a few ways to post content.
(defun post-no-auth (connection url &optional (plist nil))
  (declare (ignorable connection))
  (with-captured-dex-error 
    (let ((headers `(("Content-Type" . ,+content-type+))))
      (if plist
          (dex:post (apply #'str:concat url)
                    :headers headers
                    :content (jojo:to-json plist :from :plist)
                    :use-connection-pool nil)
          (dex:post (apply #'str:concat url)
                    :headers headers
                    :use-connection-pool nil)))))

(defun post-content (connection url content-type content)
  (with-captured-dex-error
    (dex:post (apply #'str:concat url)
              :headers (gen-headers connection content-type)
              :content content
              :use-connection-pool nil)))

(defclass empty-object ()
  ())

(defmethod jojo:%to-json ((o empty-object))
  (jojo:with-object
    nil))

(defun post-request (connection url &optional (plist nil))
  (with-captured-dex-error
    (let ((headers (gen-headers connection)))
      (if plist
          (dex:post (apply #'str:concat url)
                    :headers headers
                    :content (jojo:to-json plist :from :plist)
                    :use-connection-pool nil)
          (dex:post (apply #'str:concat url)
                    :headers headers
                    :use-connection-pool nil)))))

(defun post-request-object (connection url object)
  (with-captured-dex-error
    (let ((headers (gen-headers connection)))
      (dex:post (apply #'str:concat url)
                :headers headers
                :content object
                :use-connection-pool nil))));;do not auto convert to json

(defun admin-post-request (connection url &optional (plist nil))
  (with-captured-dex-error
    (let ((headers (gen-headers connection)))
      (if plist
          (dex:post (apply #'str:concat url)
                    :headers headers
                    :content (jojo:to-json plist :from :plist)
                    :use-connection-pool nil)
          (dex:post (apply #'str:concat url) :headers headers
                                             :use-connection-pool nil)))))

(defun admin-post-object (connection url object)
  (with-captured-dex-error
    (let ((headers (gen-headers connection)))
      (dex:post (apply #'str:concat url)
                :headers headers
                :content (jojo:to-json object)
                :use-connection-pool nil))))
  

;;;put requests 
(defun put-request (connection url plist)
  (with-captured-dex-error 
    (dex:put (apply #'str:concat url) :headers (gen-headers connection)
                                      :content  (jojo:to-json plist :from :plist)
                                      :use-connection-pool nil)))

(defun put-request-object (connection url object)
  (with-captured-dex-error 
    (dex:put (apply #'str:concat url) :headers (gen-headers connection)
                                      :content  (jojo:to-json object)
                                      :use-connection-pool nil)))

(defun put-request-from-json (connection url json-string)
  (with-captured-dex-error 
    (dex:put (apply #'str:concat url) :headers (gen-headers connection)
                                      :content  json-string
                                      :use-connection-pool nil)))

(defun admin-put-request (connection url plist)
  (with-captured-dex-error 
    (dex:put (apply #'str:concat url)
             :headers (gen-headers connection)
             :content  (jojo:to-json plist :from :plist)
             :use-connection-pool nil)))

;;;get requests      
(defun get-request (connection url &optional (get-params nil))
  (with-captured-dex-error
    (dex:get
     (if get-params
         (apply #'str:concat
                (append url (list "?") (list (plist-to-get-params get-params))))
         (apply #'str:concat url))
     :headers (gen-headers connection)
     :use-connection-pool nil)))

(defun admin-get-request (connection url)
  (let ((url (apply #'str:concat url)))
    (with-captured-dex-error 
      (dex:get url :headers (gen-headers connection)
                   :use-connection-pool nil))))


;;;delete requests
(defun admin-delete-request (connection url)
  (let ((url (apply #'str:concat url)))
    (with-captured-dex-error 
      (dex:delete url :headers (gen-headers connection)
                      :use-connection-pool nil))))


;;;request macro system
(defun new-request-type (key fun)
  (setf (gethash key *request-type-hash*) fun))

(defmacro new-r-t ((key) &body body)
  `(new-request-type ,key
                     (lambda (connection url data prefix)
                       (declare (ignorable data))
                       ,@body)))

(new-r-t (:post-no-auth)
  (jojo:parse
   (post-no-auth connection
                 (nconc (gen-url connection prefix) url)
                 data)))

(new-r-t (:post)
  (jojo:parse
   (post-request connection
                 (nconc (gen-url connection  prefix) url)
                 data)))

(new-r-t (:post-object)
  (jojo:parse
   (post-request-object connection
                        (nconc (gen-url connection prefix) url)
                        data)))

(new-r-t (:post-content)
  (jojo:parse
   (post-content connection
                 (nconc (gen-url connection  prefix) url)
                 (getf data :content-type)
                 (getf data :content))))

(new-r-t (:get)
  (jojo:parse
   (get-request connection
                (nconc (gen-url connection  prefix) url)
                data)))

(new-r-t (:content-get)
  (get-request connection
               (nconc (gen-url connection  prefix) url)
               data))

(new-r-t (:put)
  (jojo:parse
   (put-request connection
                (nconc (gen-url connection  prefix) url)
                data)))

(new-r-t (:put-event)
  (jojo:parse
   (put-request-object connection
                       (nconc (gen-url connection  prefix) url)
                       (getf data :object))))

(new-r-t (:put-content)
  (jojo:parse
   (put-request-object connection
                       (nconc (gen-url connection  prefix) url)
                       data)))

(new-r-t (:put-no-json)
  (jojo:parse
   (put-request-from-json connection
                          (nconc (gen-url connection  prefix) url)
                          data)))

(new-r-t (:admin-post)
  (declare (ignore prefix))
  (jojo:parse
   (admin-post-request connection (nconc (gen-admin-url connection) url) data)))

(new-r-t (:admin-post-object)
  (declare (ignore prefix))
  (jojo:parse
   (admin-post-request connection (nconc (gen-admin-url connection) url) data)))

(new-r-t (:admin-get)
  (declare (ignore prefix))
  (jojo:parse
   (admin-get-request connection (nconc (gen-admin-url connection) url))))

(new-r-t (:admin-put)
  (jojo:parse
   (admin-put-request connection
                      (nconc (gen-url connection  prefix) url)
                      data)))

(new-r-t (:admin-delete)
  (declare (ignore prefix))
  (jojo:parse
   (admin-delete-request connection
                         (nconc (gen-admin-url connection) url))))

;;;main macro for sending requests
(defmacro auth-req ((method connection url data response-var
                     &optional (prefix :DEFAULT)) &body body)
  (check-type url list)
  (alexandria:with-gensyms (req)
    `(let ((,req (funcall
                  (gethash ,method *request-type-hash*)
                  ,connection (list ,@url) ,data ,prefix)))
       (let ((,response-var ,req))
         ,@body))))
