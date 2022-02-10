(in-package #:lunamech-matrix-api/v2)

(defmacro with-hash-keys (keys hash &body body)
  "Creates a let binding for each of the keys listed in KEYS in HASH using gethash, 
each of these KEYS has to have a non nil value otherwise signals 'malformed-json."
  (alexandria:once-only (hash)
    `(let ,(mapcar (lambda (key)
                     `(,key (gethash ,(string key) ,hash)))
            keys)
       (locally ,@body))))

(defun destructure-mxc (mxc)
  "mxc://<server-name>/<media-id> -> server-name media-id"
  (let ((split (str:split "/" mxc :omit-nulls t)))
    (destructuring-bind (a server content)
        split
      (declare (ignore a))
      (values server content))))
