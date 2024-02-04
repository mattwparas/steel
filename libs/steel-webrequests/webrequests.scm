(#%require-dylib "libsteel_webrequests"
                 (only-in get
                          post
                          put
                          delete
                          patch
                          head
                          call
                          call-with-json
                          response->text
                          set-query-parameter!
                          set-header!
                          set-timeout/ms!
                          client/get
                          client/put
                          client/post
                          client/delete
                          client/patch
                          client/head
                          client/new))

(provide get
         post
         put
         delete
         patch
         head
         call-with-json-body
         set-query-parameter!
         set-header!
         set-timeout/ms!
         call
         response->text
         response->json
         with-query-parameter
         with-header
         with-bearer-auth
         client/get
         client/put
         client/post
         client/delete
         client/patch
         client/head
         client/new)

(define (response->json resp)
  (string->jsexpr (response->text resp)))

(define (with-query-parameter req parameter value)
  (set-query-parameter! req parameter value)
  req)

(define (with-header req header value)
  (set-header! req header value)
  req)

(define (with-bearer-auth req value)
  (with-header req "Authorization" (string-append "Bearer " value)))

(define (call-with-json-body req json)
  (call-with-json req (value->jsexpr-string json)))
