# steel/http
### **download-file!**
Download file from a URL
### **http-parse-request**
Parses an HTTP request out of the given byte vector. Returns a request
object on success, or `#false` if the buffer does not yet contain a
complete request.

(http-parse-request buf) -> (or http-request? #false)

* buf : bytes?
### **http-parse-response**
Parses an HTTP response out of the given byte vector. Returns a response
object on success, or `#false` if the buffer does not yet contain a
complete response.

(http-parse-response buf) -> (or http-response? #false)

* buf : bytes?
### **http-request-body-offset**
Returns the byte offset into the original buffer at which the body of a
parsed HTTP request begins.

(http-request-body-offset request) -> int?

* request : http-request?
### **http-request-headers**
Returns the headers of a parsed HTTP request as a hashmap mapping header
names (strings) to their values (byte vectors).

(http-request-headers request) -> hash?

* request : http-request?
### **http-request-method**
Returns the HTTP method (for example `"GET"` or `"POST"`) of a parsed
HTTP request.

(http-request-method request) -> string?

* request : http-request?
### **http-request-path**
Returns the request path of a parsed HTTP request.

(http-request-path request) -> string?

* request : http-request?
### **http-request-version**
Returns the HTTP version of a parsed HTTP request.

(http-request-version request) -> string?

* request : http-request?
### **http-response-headers**
Returns the headers of a parsed HTTP response as a hashmap mapping header
names (strings) to their values (byte vectors).

(http-response-headers response) -> hash?

* response : http-response?
