;; (defpackage #:lunamech-matrix-api/v2/11
;;   (:use #:lunamech-matrix-api/v2 #:cl)
;;   (:export #:search-for-user
;;            #:search-term
;;            #:limit

;;            #:profile%set-display-name
;;            #:user-id
;;            #:displayname

;;            #:profile%get-display-name

;;            #:profile%set-avatar-url
;;            #:avatar-url

;;            #:profile%get-avatar-url
;;            #:profile%get-profile-information))

(in-package #:lunamech-matrix-api/v2/api)

(defapi%post search-for-user ("user_directory/search")
             "Performs a search for users."
             ((search-term
               :accessor search-term
               :initarg :search-term
               :requiredp t)
              (limit
               :accessor limit
               :initarg :limit))
             (:rate-limited-p t)
             (:requires-auth-p t))

(defapi%put profile%set-display-name ("profile/:user-id/displayname")
            "Sets the users displayname"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (displayname
              :accessor displayname
              :initarg :displayname))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get profile%get-display-name ("profile/:user-id/displayname")
            "Gets the displayname"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%put profile%set-avatar-url ("profile/:user-id/avatar_url")
            "Sets the users avatar url"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t)
             (avatar-url
              :accessor avatar-url
              :initarg :avatar-url))
            (:rate-limited-p t)
            (:requires-auth-p t))

(defapi%get profile%get-avatar-url ("profile/:user-id/avatar_url")
            "Gets the users avatar url"
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))

(defapi%get profile%get-profile-information ("profile/:user-id")
            "Get the combined profile information for this user."
            ((user-id
              :accessor user-id
              :initarg :user-id
              :in-url-p t
              :requiredp t))
            (:rate-limited-p nil)
            (:requires-auth-p nil))







