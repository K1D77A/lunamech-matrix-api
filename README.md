Basic implementation of the Matrix API used in [LunaMech](https://github.com/K1D77A/LunaMech) the Matrix bot.

[Lunamech.com](https://lunamech.com)

# v2
V2 is under active development while v1 is now in maintenance only mode.
Currently v2 wraps every call in the client-server spec, the spaces api and every call in the synapse admin api.

# Important notes

When using an api call like
```lisp
(make-instance 'rooms%public-rooms/filtered :connection #v13 :include-all-networks nil)
```
Make sure you check the API documentation to see which slot does what because boolean query parameters are "true" | "false" while the boolean values for json body parameters are t | nil. You might be able to workout which is from the initial value, if its "false" | "true" you know its a query param. Maybe I will implement a translator at some point for the query params.

in `object-helpers.lisp` there are a variety of objects constructed using keyword arguments,
all arguments that default to :ne mean "no encode".

```lisp
(defun object%room-filter (&key (not-rooms :ne)(rooms :ne)(ephemeral :ne)
                             (include-leave :ne)(state :ne)(timeline :ne)
                             (account-data :ne))
  (%quick-hash `(("not_rooms" . ,not-rooms)("rooms" . ,rooms)("ephemeral" . ,ephemeral)
                 ("include_leave" . ,include-leave)("state" . ,state)("timeline" . ,timeline)
                 ("account_data" . ,account-data))))
```

The package is :lunamech-matrix-api/v2.
## see api/v2/user-api.lisp for some thin wrappers
## see api/v2/object-helpers.lisp for some Matrix object wrappers

## see api/v2/section you want/api.lisp for classes

All api calls are instances of a class, simply create an instance with make-instance 
and then use `(call-api <your instance>)` to execute. 
txnId's are automatically added, so dont fill that slot. Make sure that you always provide :connection as an initarg otherwise you will get an error. 

There is a little bit of thread safety with a connection object using `with-locked-connection` this is used when calling `password-login` and `logout` and every time a call is made that
uses a txn the lock is grabbed to increment the txn and set it that within in the call.
Other than that there is no other thread safety, however this is more than v1.

In theory when you use (call-api <obj>) it will error if you are missing some required values, it will also serialize slots are query parameters and path parameters correctly, 
when you make the object you should be able to see if a required slot is missing in the 
printed representation of the object in the REPL. You can also see the json, and the 
potential url.

You can resend the same event and the previous result will be overridden (results are stored in the slot res) and the txn will still be incremented.

cl-json is used to encode all json (body) values. 

When encoding query params nil and unbound slots are ignored, if you want something like 'false' then set the slot to false. 

Some api calls are 'special' and will have one slot that is declared specialp this just means that the content of this slot is encoded to json and sent rather than the normal slots, if you fail to fill a special slot a condition will be signalled.

## A few examples
```lisp
CL-USER> (ql:quickload :lunamech-matrix-api)
To load "lunamech-matrix-api":
  Load 1 ASDF system:
    lunamech-matrix-api
; Loading "lunamech-matrix-api"

(:LUNAMECH-MATRIX-API)
CL-USER> (in-package #:lunamech-matrix-api/v2)
#<PACKAGE "LUNAMECH-MATRIX-API/V2">
LMAV2> (make-instance 'connection :url "https://"
                                  :api "/_matrix/client/r0/" :username ""
                                  :password "")
#<CONNECTION 
URL: ""
Username: ""
Logged in: NIL
Auth: "Not authorized yet"
Device-id: "No device ID yet"
 {100C35A453}>
LMAV2> (password-login *)
#<CONNECTION 
URL: "https://"
Username: ""
Logged in: T
Auth: #<AUTH {100E4C88E3}>
Device-id: "VIFOADZRRE"
 {100C35A453}>
LMAV2> 

```

### Sending a message to a room from repl 

```lisp
LMAV2> (multiple-value-bind (hash type)
           (object%event/m-room-message "foobarquux")
         (make-instance 'events%put-message-event-into-room
                        :body hash
                        :room-id "<the room id>"
                        :event-type type
                        :connection #v6))
#<EVENTS%PUT-MESSAGE-EVENT-INTO-ROOM 
PUT https://matrix.scyldings.com/_matrix/client/r0/rooms/%21Wom/send/m.room.message/0
Content-Type: application/json; charset=utf-8
JSON: {"msgtype":"m.text","body":"foobarquux"}
MISSING: NONE {100FA9E78B}>
LMAV2> (call-api *)
(:|event_id| "$feWh-tyFUlqzJD0Gamaa0ndszoENf9F0GwJqe5CIChY")
#<EVENTS%PUT-MESSAGE-EVENT-INTO-ROOM 
PUT https://matrix./_matrix/client/r0/rooms/<the room id>/send/m.room.message/1
Content-Type: application/json; charset=utf-8
JSON: {"msgtype":"m.text","body":"foobarquux"}
MISSING: NONE {100FA9E78B}>
LMAV2> 
```

### Retracting that same event

```lisp
LMAV2> (make-instance 'events%redact-event
                      :reason "Test"
                      :room-id "!WJvFXSrAnfoqNgwqpE:scyldings.com"
                      :event-id (getf * :|event_id|)
                      :connection #v6)
#<EVENTS%REDACT-EVENT 
PUT https://matrix.scyldings.com/_matrix/client/r0/rooms/%2s.com/redact/%24feWh-tyFUlqzJD0Gamaa0ndszoENf9F0GwJqe5CIChY/1
Content-Type: application/json; charset=utf-8
JSON: {"reason":"Test"}
MISSING: NONE {10042E8DDB}>
LMAV2> (call-api *)
(:|event_id| "$Z-19Wv6N1Xm21p0wFBHuqMI-XzWRtqQoX0Vy2GMEEFU")
#<EVENTS%REDACT-EVENT 
PUT https://matrix.scyldings.com/_matrix/client/r0/rooms/s.com/redact/%24feWh-tyFUlqzJD0Gamaa0ndszoENf9F0GwJqe5CIChY/2
Content-Type: application/json; charset=utf-8
JSON: {"reason":"Test"}
MISSING: NONE {10042E8DDB}>
LMAV2> 
```
## Filters and syncing

Make the filter object, this is a normal api object made normally with `generate-user-room-filter` or `(make-instance 'filters%upload ..)` You can see an example in filters.lisp, the objects from the spec are in objects.lisp.
```lisp
LMAV2> (filter-to-remove-receipts-reaction-typing #v13 "@om")
#<FILTERS%UPLOAD 
POST https://matrix..m/_matrix/client/r0/user/%om/filter
Content-Type: application/json; charset=utf-8
JSON: {"event_format":"client","presence":{"not_types":["m.presence"]},"room":{"ephemeral":{"not_types":["m.room.*","m.receipt","m.typing","m.reaction"]}}}
MISSING: NONE {100790EECB}>
```
Then call `(upload-new-filter <key> <filter-object>)`
```lisp
LMAV2> (upload-new-filter :junk-removed *)
(#<FILTER {100790A193}>)
```
You can now sync using that filter (ofcourse you can sync without a filter).

```lisp
(sync #v13 :filter "9")
```
Or using (key-sync <con> <key>)

```lisp
LMAV2> (key-sync #v13 :junk-removed)
(:|org.matrix.msc2732.device_unused_fallback_key_types| NIL
 :|device_one_time_keys_count| (:|signed_curve25519| 0) :|next_batch|
 "s1694008_20710893_241342_3874091_2715649_2558_1533518_732813_698")
 ```



## Retrying
Every API call is wrapped with the macro 'with-captured-dex-error' (see api/v2/protocol/call-wrapper.lisp) this macro wraps its body with a bt:with-timeout and catches the bt:timeout and signalling a api-timeout condition, it also catches all other conditions and passes them to a method called `%call-condition-handler`, a variety of conditions are handled elegantly and are converted into specific subclass of api-error, however if no method is found for the signalled condition then the top-level condition of 'api-error is signalled.
The macro also provides a restart called 'try-again', this will execute the body of `with-captured-dex-error` again. This can be invoked like so: 

```lisp
LMAV2> (handler-bind ((condition (lambda (c) (declare (ignore c))
                                   (print "restarting")
                                   (invoke-restart 'try-again))))
         (with-captured-dex-error (sleep 2)))

"restarting" 
"restarting" 
```
(by default the timeout is 30 seconds, it was reduced to 1 just for this demonstration)

So for example if you are running a loop where it constantly checks for a new sync, you can wrap that call or worker thread with a handler-bind which invokes restart, perhaps you could try logging in again etc and then restarting the thread where it was without having unwound the stack. You can see the default way conditions signalled by api-calls are handled in the previously mentioned files.

# v1
## See api/user-api 
## See api/admin-api
## See api/spaces

I have some plans to change the backend to use drakma rather than dex and I might move over to using the MOP for defining new API's. However I am very dependent on the current APIs interface so this will have to stay the same. 

```lisp
LMAPI> (make-instance 'connection :url "<your matrix server address>"
                                  :api "/_matrix/client/r0/" :username "<your username>"
                                  :password "<your password>")
#<CONNECTION 
URL: "https://_matrix/client/r0/"
Username: "lh"
Logged in: NIL
Auth: "Not authorized yet"
Device-id: "No device ID yet"
 {10080B0C43}>
LMAPI> (password-login *)
(:|well_known| (:|m.homeserver| (:|base_url| "htm/"))
 :|device_id| "XLXMACKAFI" :|home_server| "scom" :|access_token|
 "sytdW" :|user_id|
 "@lom")
LMAPI> 
; Returning value 0 of history entry 21
#<CONNECTION 
URL: "https://m.com/_matrix/client/r0/"
Username: "h"
Logged in: T
Auth: #<AUTH {10093A81E3}>
Device-id: "XLXMACKAFI"
 {10080B0C43}>
 ```
Now you can pass that connection as the first argument to api calls.
