(require "slack.scm")
(require "steel/time/time.scm"
         (for-syntax "steel/time/time.scm"))

(require "steel/logging/log.scm")

(define/contract (process-message body)
  (->/c hash? any/c)

  (log/info! body)

  (define event-json (-> body (hash-get 'payload) (hash-get 'event)))
  (define text (hash-try-get event-json 'text))
  (define channel (hash-get event-json 'channel))

  (when (and text (starts-with? text "!ping"))
    (time! (send-message channel "pong!"))))

(define (process-message-timed body)
  (time! (process-message body)))

(define *ws-url* (get-ws-url))

(event-loop *ws-url* (connect-to-slack-socket *ws-url*) process-message-timed)
