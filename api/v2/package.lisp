(defpackage #:lunamech-matrix-api/v2
  (:use #:cl)
  (:nicknames #:lmav2)
  (:export #:defapi
           #:defapi%post
           #:defapi%get
           #:defapi%delete
           #:defapi%put
           #:connection))
