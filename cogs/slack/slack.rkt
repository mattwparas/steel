(require-builtin steel/web/requests)
(require-builtin steel/web/ws)
(require "steel/result")

(define client (request/client))

(define (env-var! var) (unwrap-ok (env-var var)))

(define *SLACK_API_TOKEN* (env-var! "SLACK_API_TOKEN"))
(define *SLACK_API_WS_TOKEN* (env-var! "SLACK_API_WS_TOKEN"))

(define *post-message-url* "https://slack.com/api/chat.postMessage")
(define *ws-connection-url* "https://slack.com/api/apps.connections.open")

(define (send-message channel content)
    (~> client
        (client/post *post-message-url*)
        (request/bearer-auth *SLACK_API_TOKEN*)
        (request/json (hash 'channel channel 'text content))
        (request/send)
        (unwrap-ok)))

(define (get-ws-url)
    (~> client
        (client/post *ws-connection-url*)
        (request/bearer-auth *SLACK_API_WS_TOKEN*)
        (request/json (hash))
        (request/send)
        (unwrap-ok)
        (response->json)
        (unwrap-ok)
        (hash-get 'url)))

(define *ws-url* (get-ws-url))

(define (connect-to-slack-socket)
    (~> *ws-url*
        (ws/connect)
        (unwrap-ok)
        (first)))

(define socket (connect-to-slack-socket))

(define (process-message body)
  (displayln body)
  (define event-json (-> body (hash-get 'payload) (hash-get 'event)))
  (define text (hash-get event-json 'text))
  (define channel (hash-get event-json 'channel))
  (when (starts-with? text "!ping")
        (send-message channel "pong!")))

(define (send-acknowledgement socket body)
    (ws/write-message! socket
        (ws/message-text 
            (value->jsexpr-string 
                (hash 'envelope_id (hash-get body 'envelope_id))))))

(define (loop message-thunk)
  (define message (ws/read-message! socket))
  (cond [(Err? message) => 
                (displayln "Unable to read the message from the socket, retrying connection")
                ;; Try to reconnect and see what happens
                ;; Probably need to add a sleep here at some point to retry with a backoff
                (set! socket (connect-to-slack-socket))
                (loop message-thunk)]
        [else => 
          ;; At this point, the message should be guaranteed to be here, unwrap and continue
          (define message (unwrap-ok message))
          (displayln message)
          ;; If its a ping, respond with a pong
          (cond [(ws/message-ping? message) 
                    =>
                      (ws/write-message! socket (ws/message-ping->pong message))
                      (loop message-thunk)]
                ;; If its a text message, check if its a hello message - otherwise, continue
                ;; And process the message
                [(ws/message-text? message) 
                    =>
                      (define body (string->jsexpr (ws/message->text-payload message)))
                      (cond [(equal? "hello" (hash-try-get body 'type)) => 
                              (loop message-thunk)]
                            [else
                                => 
                                  (send-acknowledgement socket body)
                                  (message-thunk body)
                                  (loop message-thunk)])]
                [else => (loop message-thunk)])]))


(loop process-message)