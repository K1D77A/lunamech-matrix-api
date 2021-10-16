(in-package #:lunamech-matrix-api/v2)

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
       (let ((fun (lambda ()
                    (handler-case
                        (locally (bt:with-timeout (30)
                                   ,@body))
                      (bt:timeout (,condition)
                        ;;this is here because bt:timeout is a type but not a class..
                        (error 'api-timeout :api-timeout-message "Connection broken"
                                            :api-timeout-condition ,condition))
                      (condition (,condition)
                        (%call-condition-handler ,condition))))))
         (try-again-restart fun)))))


(defgeneric %call-condition-handler (condition)
  (:documentation "Attempts to handle conditions that come about as a consequence of 
executing an api call."))

(defmethod no-applicable-method ((fun (eql #'%call-condition-handler)) &rest args)
  (error 'api-error
         :api-error-error (first args)
         :api-error-code "unhandled"
         :api-error-args args
         :api-error-description "A request was made that generated a condition which 
has returned an unknown condition."))

(defmethod %call-condition-handler ((condition usocket:socket-condition))
  (error 'api-no-connection :api-timeout-message "No network. Socket-condition."
                            :api-timeout-condition condition))

(defmethod %call-condition-handler ((condition usocket:ns-error))
  (error 'api-no-connection :api-timeout-message "No network. ns-error."
                            :api-timeout-condition condition))

(defmethod %call-condition-handler ((condition dexador.error:http-request-failed))
  (%handle-dex-condition condition (dex:response-status condition)))

(defgeneric %handle-dex-condition (condition status)
  (:documentation "Attempts to correctly handle dex problems."))

(defmethod %handle-dex-condition (condition (status (eql 404)))
  (error 'api-no-connection
         :api-timeout-message "Server probably down"
         :api-timeout-condition condition))

(defmethod %handle-dex-condition (condition (status (eql 405)))
  (error 'api-error
         :api-error-error condition
         :api-error-code "Bad method"
         :api-error-args condition
         :api-error-description "A request was made with a method that is not allowed."))

(defmethod %handle-dex-condition (condition status)
  (signal-condition-from-response
   (jojo:parse (dexador.error:response-body condition))))







