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
                          set-timeout/ms!))

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
         with-header)

(define (response->json resp)
  (string->jsexpr (response->text resp)))

(define (with-query-parameter req)
  (set-query-parameter! req)
  req)

(define (with-header req)
  (set-header! req)
  req)

(define (call-with-json-body req json)
  (call-with-json req (value->jsexpr-string json)))
