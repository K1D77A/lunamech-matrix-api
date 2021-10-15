(in-package #:lunamech-matrix-api/v2)

(defapi%get discover-domain-info ("")
  "Gets discovery information about the domain."
  ();tested
  (:rate-limited-p nil)
  (:requires-auth-p nil)
  (:api "/.well-known/matrix/client"))
