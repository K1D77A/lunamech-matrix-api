(defpackage #:lunamech-matrix-api/v2/api
  (:use #:CL #:lunamech-matrix-api/v2)
  (:nicknames #:v2/api)
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
           #:username
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
           #:server-name
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
           #:event-type
           #:txn
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
           #:new-version))



