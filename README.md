Basic implementation of the Matrix API used in [LunaMech](https://github.com/K1D77A/LunaMech) the Matrix bot.

[Lunamech.com](https://lunamech.com)

# v2
V2 is under active development while v1 is now in maintenance only mode.
Currently v2 wraps every call in the spec, the spaces api and every call in the synapse admin api.

The package is :lunamech-matrix-api/v2.
## see api/v2/user-api.lisp for some thin wrappers
## see api/v2/object-helpers.lisp for some Matrix object wrappers

## see api/v2/section you want/api.lisp for classes

All api calls are instances of a class, simply create an instance with make-instance 
and then use `(call-api <your instance>)` to execute. 
txnId's are automatically added, so dont fill that slot. Make sure that you always provide :connection as an initarg otherwise you will get an error. 

There is a little bit of thread safety with a connection object using `with-locked-connection` this is used when calling `password-login` and `logout` and every time a call is made that
uses a txn the lock is grabbed to increment the lock and set it. Other than that there is 
no other thread safety, however this is more than v1.


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
