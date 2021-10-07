;; (defpackage #:lunamech-matrix-api/v2/4
;;   (:use #:lunamech-matrix-api/v2/api #:cl)
;;   (:export #:discover-domain-info))

(in-package #:lunamech-matrix-api/v2/api)

(defapi%get discover-domain-info ("")
            "Gets discovery information about the domain."
            ();tested
            (:rate-limited-p nil)
            (:requires-auth-p nil)
            (:api "/.well-known/matrix/client"))
