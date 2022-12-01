(require-builtin steel/requests)
(require "std::result")

(define client (Client))

(define (send-message channel content)
  (post
   client
   "https://slack.com/api/chat.postMessage"
   `((content-type "application/json")
     (authorization ,(string-append "Bearer " (unwrap-ok (env-var "SLACK_API_TOKEN")))))
   (hash
    'channel channel
    'text content)))

(define (get-ws-url)
  (post
   client
   "https://slack.com/api/apps.connections.open"
   `((content-type "application/x-www-form-urlencoded")
     (authorization ,(string-append "Bearer " (unwrap-ok (env-var "SLACK_API_WS_TOKEN")))))
   (hash)))

(define ws-url (-> (get-ws-url)
                   (unwrap-ok)
                   (hash-get 'url)))

(define socket (unwrap-ok (ws/connect ws-url)))

(define (process-message body)
  (displayln body)
  (define event-json (-> body (hash-get 'payload) (hash-get 'event)))
  (define text (hash-get event-json 'text))
  (define channel (hash-get event-json 'channel))
  (when (starts-with? text "!ping")
        (send-message channel "pong!")))

(define (send-acknowledgement socket body)
    (ws/write-message socket
        (new-message-text (value->jsexpr-string (hash 'envelope_id (hash-get body 'envelope_id))))))

(define (loop)
  (define message (ws/read-message socket))
  (cond [(Err? message) => 
                (displayln "Unable to read the message from the socket, retrying connection")
                ;; Try to reconnect and see what happens
                ;; Probably need to add a sleep here at some point to retry with a backoff
                (set! socket (unwrap-ok (ws/connect ws-url)))
                (loop)]
        [else => 
          ;; At this point, the message should be guaranteed to be here, unwrap and continue
          (define message (unwrap-ok message))
          (displayln message)
          ;; If its a ping, respond with a pong
          (cond [(message-ping? message) 
                    =>
                      (ws/write-message socket (ping->pong message))
                      (loop)]
                ;; If its a text message, check if its a hello message - otherwise, continue
                ;; And process the message
                [(message-text? message) 
                    =>
                      (define body (string->jsexpr (message-text message)))
                      (cond [(equal? "hello" (hash-try-get body 'type)) => 
                              (loop)]
                            [else
                                => 
                                  (send-acknowledgement socket body) 
                                  (process-message body)
                                  (loop)])]
                [else => (loop)])]))


(loop)
