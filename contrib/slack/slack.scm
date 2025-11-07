(require (only-in "steel-webrequests/webrequests.scm"
                  client/new
                  client/post
                  with-bearer-auth
                  call-with-json-body
                  call
                  response->json))

(#%require-dylib "libsteel_websockets"
                 (only-in ws/message-ping?
                          ws/message-pong?
                          ws/message-text
                          ws/message-text?
                          ws/message-ping->pong
                          ws/message->text-payload
                          ws/connect
                          ws/read-message!
                          ws/write-message!))

(require "steel/result")
(require "steel/logging/log.scm")
(require-builtin steel/time)

(provide event-loop
         send-message
         connect-to-slack-socket
         get-ws-url)

(define (env-var! var)
  (let ([e (maybe-get-env-var var)])
    (if (Err? e)
        "TODO"
        (unwrap-ok e))))

(define client (client/new))

(define *SLACK_API_TOKEN* (env-var! "SLACK_API_TOKEN"))
(define *SLACK_API_WS_TOKEN* (env-var! "SLACK_API_WS_TOKEN"))

(define *post-message-url* "https://slack.com/api/chat.postMessage")
(define *ws-connection-url* "https://slack.com/api/apps.connections.open")

(define (send-message channel content)
  (~> client
      (client/post *post-message-url*)
      (with-bearer-auth *SLACK_API_TOKEN*)
      (call-with-json-body (hash 'channel channel 'text content))))

(define (get-ws-url)
  (log/info! "Requesting a websocket url")
  (~> client
      (client/post *ws-connection-url*)
      (with-bearer-auth *SLACK_API_WS_TOKEN*)
      (call)
      ; (call-with-json-body (hash))
      response->json
      (hash-get 'url)))

(define (connect-to-slack-socket url)
  (~> url (ws/connect) (first)))

(define (send-acknowledgement socket body)
  (ws/write-message! socket
                     (ws/message-text (value->jsexpr-string (hash 'envelope_id
                                                                  (hash-get body 'envelope_id))))))

(define (loop url socket message-thunk)
  (define message
    (with-handler (lambda (err)
                    (displayln "Unable to read the message from the socket, retrying connection")
                    ;; Try to reconnect and see what happens
                    ;; Probably need to add a sleep here at some point to retry with a backoff
                    (loop url (connect-to-slack-socket (get-ws-url)) message-thunk))
                  (ws/read-message! socket)))

  (log/info! message)
  ;; If its a ping, respond with a pong
  (cond
    [(ws/message-ping? message)
     (ws/write-message! socket (ws/message-ping->pong message))
     (loop url socket message-thunk)]
    ;; If its a text message, check if its a hello message - otherwise, continue
    ;; And process the message
    [(ws/message-text? message)
     (define body (string->jsexpr (ws/message->text-payload message)))
     (cond
       [(equal? "hello" (hash-try-get body 'type)) (loop url socket message-thunk)]

       [(equal? "disconnect" (hash-try-get body 'type))
        (log/info! "Refreshing the connection, sleeping for 500 ms")
        (time/sleep-ms 500)
        (loop url (connect-to-slack-socket (get-ws-url)) message-thunk)]

       [else
        (send-acknowledgement socket body)
        (message-thunk body)
        (loop url socket message-thunk)])]
    [else (loop url socket message-thunk)]))

(define event-loop loop)
