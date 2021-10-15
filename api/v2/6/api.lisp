(in-package #:lunamech-matrix-api/v2)

(defapi%get get-relevant-capabilities ("capabilities")
  "Gets information about the server's supported feature set and other relevant capabilities."
  ();tested 
  (:requires-auth-p t)
  (:rate-limited-p t))
