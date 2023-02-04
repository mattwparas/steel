(require-builtin steel/web/requests)
(require-builtin steel/web/ws)
(require "steel/result")
(require "steel/logging/log.scm")

(provide event-loop send-message connect-to-slack-socket get-ws-url)

; (define (env-var! var) (unwrap-ok (env-var var)))

(define (env-var! var)
  (let ((e (env-var var)))
    (if (Err? e)
        "TODO"
        (unwrap-ok e))))

(define client (request/client))


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

; (define *ws-url* (get-ws-url))

(define (connect-to-slack-socket url)
    (~> url
        (ws/connect)
        (unwrap-ok)
        (first)))

(define (send-acknowledgement socket body)
    (ws/write-message! socket
        (ws/message-text 
            (value->jsexpr-string 
                (hash 'envelope_id (hash-get body 'envelope_id))))))

(define (loop url socket message-thunk)
  (define message (ws/read-message! socket))
  (cond [(Err? message) => 
                (displayln "Unable to read the message from the socket, retrying connection")
                ;; Try to reconnect and see what happens
                ;; Probably need to add a sleep here at some point to retry with a backoff
                (loop url (connect-to-slack-socket url) message-thunk)]
        [else => 
          ;; At this point, the message should be guaranteed to be here, unwrap and continue
          (define message (unwrap-ok message))
          (log/info! message)
          ;; If its a ping, respond with a pong
          (cond [(ws/message-ping? message)
                    =>
                      (ws/write-message! socket (ws/message-ping->pong message))
                      (loop url socket message-thunk)]
                ;; If its a text message, check if its a hello message - otherwise, continue
                ;; And process the message
                [(ws/message-text? message) 
                    =>
                      (define body (string->jsexpr (ws/message->text-payload message)))
                      (cond [(equal? "hello" (hash-try-get body 'type)) => 
                              (loop url socket message-thunk)]

                            [(equal? "disconnect" (hash-try-get body 'type)) =>
                              (log/info! "Refreshing the connection")
                              (loop url (connect-to-slack-socket url) message-thunk)]

                            [else
                                => 
                                  (send-acknowledgement socket body)
                                  (message-thunk body)
                                  (loop url socket message-thunk)])]
                [else => (loop url socket message-thunk)])]))

(define event-loop loop)