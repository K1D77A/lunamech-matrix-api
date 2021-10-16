(defpackage #:lunamech-matrix-api/v2
  (:use #:CL)
  (:nicknames #:lmav2)
  (:export #:client-versions
           ;;2 ^
           ;;4
           #:discover-domain-info
           
           #:get-supported-logins

           #:login-connection
           #:login-type
           #:identifier
           #:password
           #:initial-device-display-name
           #:device-id

           #:logout-connection
           #:logout-all-connections

           #:register-account
           #:kind
           #:auth
           #:password
           #:device-id
           #:inhibit-login

           #:register-account/email/request-token
           #:client-secret
           #:email
           #:send-attempt
           #:next-link
           #:id-server
           #:id-access-token

           #:register-account/msisdn/request-token
           #:country
           #:phone-number

           #:change-password
           #:new-password
           #:logout-devices

           #:change-password/email/request-token

           #:change-password/msisdn/request-token

           #:deactivate-account

           #:username-avaiable

           #:3pid%get

           #:3pid%add
           #:sid

           #:3pid%bind
           
           #:3pid%delete
           #:medium
           #:address

           #:3pid%unbind

           #:3pid%email/request-token

           #:3pid%msisdn/request-token

           #:whoami
           ;;6
           #:get-relevant-capabilities
           ;;8

           #:filters%upload
           
           #:filters%download
           #:user-id
           #:event-fields
           #:event-format
           #:presence
           #:account-data
           #:room-filter

           #:filter-id

           ;;9
           #:sync
           #:filter
           #:since
           #:full-state
           #:set-presence
           #:timeout

           #:events%get-from-id
           #:room-id
           #:event-id

           #:events%get-from-type-with-statekey
           #:event-type
           #:state-key

           #:events%get-state-events-in-room

           #:events%get-room-members
           #:at
           #:membership
           #:not-membership

           #:events%get-joined-members

           #:events%get-room-messages
           #:from
           #:to
           #:dir
           #:limit
           #:filter

           #:events%put-state-event-into-room
           #:body

           #:events%put-message-event-into-room
           #:txn

           #:events%redact-event
           #:reason
           ;;10

           #:create-room
           #:visibility
           #:room-alias-name
           #:name
           #:topic
           #:invite
           #:invite-3pid
           #:room-version
           #:creation-content
           #:initial-state
           #:preset
           #:is-direct
           #:power-level-content-override

           #:alias%new-room-alias
           #:room-alias
           #:room-id

           #:alias%resolve-room-alias

           #:alias%delete-room-alias

           #:alias%list-a-rooms-aliases

           #:rooms%my-joined-rooms

           #:rooms%invite-user-to-room
           #:user-id
           
           #:rooms%join-a-room
           #:third-party-signed

           #:rooms%join-a-room/alias-or-id
           #:room-id-or-alias
           #:third-party-signed

           #:rooms%leave-a-room

           #:rooms%kick-user-from-room
           #:reason
           
           #:rooms%ban-user-from-room

           #:rooms%unban-user-from-room

           #:rooms%set-room-visibility

           #:rooms%public-rooms
           #:limit
           #:since
           #:server

           #:rooms%public-rooms/filtered
           #:filter
           #:include-all-networks
           #:third-party-instance-id

           ;;11
           #:search-for-user
           #:search-term
           #:limit
           
           #:profile%set-display-name
           #:user-id
           #:displayname

           #:profile%get-display-name

           #:profile%set-avatar-url
           #:avatar-url

           #:profile%get-avatar-url
           #:profile%get-profile-information

           ;;13

           #:get-turnserver-credentials
           #:username
           #:password
           #:urls
           #:ttl

           #:rooms%put-typing-notification
           #:user-id
           #:room-id
           #:typing
           #:timeout

           #:rooms%send-event-receipt
           #:receipt-type
           #:event-id

           #:presence%set-presence
           
           #:presence%get-presence

           #:media%upload
           #:content-type
           #:filename
           #:bytes

           #:media%get-media
           #:server-name
           #:media-id
           #:allow-remote

           #:media%get-media/filename

           #:media%get-thumbnail
           #:width
           #:height
           #:resize-method

           #:media%get-preview
           #:url
           #:ts

           #:media%get-config

           #:protocol%send-to-device
           #:messages

           #:devices%get-devices

           #:devices%get-device
           #:device-id

           #:devices%update-device
           #:display-name

           #:devices%delete-device
           #:auth

           #:devices%delete-devices
           #:devices

           #:keys%upload-keys
           #:device-keys
           #:one-time-keys

           #:keys%download-devices-and-keys
           #:token

           #:keys%claim-keys

           #:keys-get-key-changes
           #:from
           #:to

           #:pushers%get-active-pushers
           #:pushers

           #:pushers%set-pusher
           #:pushkey
           #:kind
           #:app-id
           #:app-display-name
           #:device-display-name
           #:lang
           #:data
           #:append-bool

           #:notifications%get-notifications
           #:only
           #:limit

           #:pushrules%get-pushrules
           #:global
           #:drill-down

           #:pushrules%get-specific-pushrule
           #:scope
           #:rule-id

           #:pushrules%delete-specific-pushrule

           #:pushrules%create-pushrule
           #:after
           #:actions
           #:conditions
           #:pattern 

           #:pushrules%pushrule-enabled

           #:pushrule%enable-pushrule
           #:enabled

           #:pushrules%pushrule-actions

           #:pushrules%change-a-pushrule-actions

           #:rooms%invite-user-to-room/3pid
           #:id-server
           #:id-access-token
           #:medium
           #:address

           #:server-side-search
           #:next-batch
           #:search-categories

           #:wait-for-events

           #:tags%list-tags

           #:tags%set-tags
           #:tag
           #:order

           #:tags%delete-tag

           #:account-data%set-data
           #:data-type

           #:account-data%get-data

           #:account-data%set-data-in-room

           #:account-data%get-data-in-room

           #:admin%whois-user

           #:rooms%events-before-and-after
           #:filter

           #:sso%sso-url
           #:redirect-url

           #:rooms%report-content
           #:score
           #:reason

           #:thirdparty%get-protocols-metadata

           #:thirdparty%get-protocol-metadata
           #:protocol

           #:thirdparty%get-protocol-users
           #:fields

           #:thirdparty%get-thirdparty-locations
           #:alias

           #:thirdparty%thirdparty-for-user

           #:openid%request-openid

           #:rooms%upgrade-room
           #:new-version

           ;;admin
           #:admin%renew-account
           #:expiration-ts
           #:enable-renewal-emails

           #:admin%delete-group
           #:group-id

           #:admin%get-event-reports

           #:admin%get-specific-event-report
           #:report-id

           #:admin%quarantine-media-by-id

           #:admin%unquarantine-media-by-id

           #:admin%quarantine-media-in-room

           #:admin%quarantine-users-media

           #:admin%protect-media-by-id

           #:admin%unprotect-media-by-id

           #:admin%delete-media-by-id

           #:admin%delete-media-by-date-or-size
           #:before-ts
           #:size-gt
           #:keep-profiles

           #:admin%purge-remote-media-cache
           #:unix-timestamp-in-ms

           #:admin%purge-room-history
           #:delete-local-events
           #:purge-up-to-event-id
           #:purge-up-to-ts

           #:admin%get-purge-status
           #:purge-id

           #:admin%register-user
           #:nonce
           #:mac

           #:admin%get-one-token

           #:admin%create-token
           #:uses-allowed
           #:expiry-time
           #:token-length

           #:admin%update-token

           #:admin%delete-token

           #:admin%edit-users-room-membership

           #:admin%list-rooms

           #:admin%get-room-details

           #:admin%get-room-members

           #:admin%get-room-state

           #:admin%delete-room
           #:new-room-user-id
           #:room-name
           #:block-room
           #:purge
           #:force-purge

           #:admin%make-user-admin-in-room

           #:admin%get-room-forward-extremities

           #:admin%delete-room-forward-extremities

           #:admin%get-event-context

           #:admin%post-server-notice
           
           #:admin%put-server-notice

           #:admin%get-users-media-statistics
           #:order-by
           #:until-ts
           #:from-ts

           #:admin%get-server-version

           #:admin%query-user-account

           #:admin%modify-user-account
           #:threepids
           #:external-ids
           #:deactivated
           #:admin

           #:admin%list-accounts
           #:guests

           #:admin%query-current-sessions

           #:admin%deactivate-account
           #:erase

           #:admin%reset-password
           #:logout-devices
           #:new-password

           #:admin%get-user-admin-status

           #:admin%set-user-admin-status

           #:admin%get-users-room-memberships

           #:admin%get-users-uploaded-media

           #:admin%delete-users-media

           #:admin%login-as-user

           #:admin%get-users-devices

           #:admin%delete-users-devices
           #:devices

           #:admin%show-user-device
           #:device-id

           #:admin%update-user-device

           #:admin%delete-user-device

           #:admin%get-users-pushers

           #:admin%shadowban-user

           #:admin%get-users-ratelimit

           #:admin%set-users-ratelimit
           #:messages-per-second
           #:burst-count

           #:admin%delete-users-ratelimit

           #:admin%check-username-is-available

           #:spaces%space-hierarchy
           #:suggested-only
           #:max-depth
           ;;user-api 
           #:password-login

           #:logout

           #:public-rooms

           #:get-room-state

           #:join-room

           #:leave-room

           #:joined-rooms

           #:send-message-to-room

           #:send-message-event-to-room

           #:send-event-to-room

           #:redact-event-in-room

           #:kick-user-from-room

           #:user-display-name

           #:valid-user-p

           #:ban-user-from-room

           #:unban-user-from-room

           #:members-in-room

           #:members-in-room-ids

           #:upload-content

           #:send-image-file-to-room

           #:send-image-bytes-to-room

           ;;classes
           #:status
           #:latest-sync
           
           #:connection
           #:logged-in-p
           #:filters
           #:status
           #:url
           #:api
           #:username
           #:txn
           #:user-id
           #:password
           #:auth
           #:encryption
           #:con-lock
           #:device-id

           #:with-locked-connection

           #:make-connection

           #:encryption
           #:olm-account
           #:server-otk

           #:auth
           #:token

           #:filter
           #:key
           #:id
           #:last-sync-string
           #:next-sync-string
           ;;conditions
           #:lunamech-matrix-api-condition
           #:api-error
           #:api-error-error
           #:api-error-code
           #:api-error-args
           #:api-error-description

           #:api-timeout
           #:api-timeout-message
           #:api-timeout-condition

           #:api-no-connection

           #:m-forbidden
           #:m-unknown-token
           #:m-missing-token
           #:m-bad-json
           #:m-not-json
           #:m-not-found
           #:m-limit-exceeded
           #:m-unknown
           #:m-unrecognized
           #:m-unauthorized
           #:m-invalid-param
           #:m-room-in-use
           #:m-bad-state
           ;;sync 
           #:sync
           
           #:traverse-sync
           
           #:room-timeline
           
           #:room-messages
           
           #:membership-events
           
           #:room-leaves

           #:room-joins

           #:room-invite

           #:extract-events-of-type
           ;;integration
           #:dimension-connection

           #:dimension-api

           #:integration%register
           #:access-token
           #:matrix-server-name
           #:token-type
           #:expires-in
           
           #:integration%account

           #:integration%register

           #:integration%user-information

           #:integration%validate-user-id
           ;;object-helpers
           #:%quick-hash

           #:object%identifier-type/m-id-user

           #:object%identifier-type/m-id-thirdparty

           #:object%identifier-type/m-id-phonenumber

           #:object%event/m-room-redaction

           #:object%event/m-room-message/m-text%basic

           #:object%event/m-room-message/m-text

           #:object%image-info

           #:object%file-info

           #:object%thumbnail-info

           #:object%event/m-room-message/m-image
           ;;protocol/conditions
           #:api-protocol-condition
           #:message

           #:problems-with-special

           #:set-special

           #:special-slot-is-not-bound

           #:connection-unbound
           #:obj

           #:api-protocol-condition
           #:slot
           ;;protocol/api-protocol
           #:call-api

           #:api
           #:result

           #:url-e
           ))
