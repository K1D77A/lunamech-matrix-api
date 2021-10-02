(defpackage #:lunamech-matrix-api/v2/2
  (:use #:lunamech-matrix-api/v2 #:cl))

(in-package #:lunamech-matrix-api/v2/2)

(defapi%get client-versions ("versions")
            "Gets the versions of the specification supported by the server."
            ();tested
            (:rate-limited-p nil)
            (:requires-auth-p nil)
            (:api "/_matrix/client/"))

