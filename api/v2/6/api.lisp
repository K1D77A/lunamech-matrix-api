(defpackage #:lunamech-matrix-api/v2/6
  (:use #:lunamech-matrix-api/v2 #:cl)
  (:export #:get-relevant-capabilities))

(in-package #:lunamech-matrix-api/v2/6)

(defapi%get get-relevant-capabilities ("capabilities")
            "Gets information about the server's supported feature set and other relevant capabilities."
            ();tested 
            (:requires-auth-p t)
            (:rate-limited-p t))
