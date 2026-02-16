(require-builtin steel/polling)
(require-builtin steel/tcp)

(require "steel/lists/lists.scm")
(require "green-threads.scm")

(define listener (tcp-listen "0.0.0.0:8080"))

;; Insert key -> cont
(define *async-events* (hash))

;; Set the listener to be non blocking, and we're going
;; to set up an event loop.
(tcp-listener-set-non-blocking! listener)

(define key 7)
(define poller (make-poller))

;; Register interest with this listener
(add-event-interest-read poller listener key)

;; Events is going to be thread local.
;; So we will clear it per loop, but we won't be
;; responsible for setting up the events buffer itself.

(define (event-loop)
  (events-clear!)
  ;; Probably need some global poller
  (poller-wait poller)

  (let ([events (events->list)])
    (for-each (lambda (event)
                (when (= event key)
                  (displayln "Performing a non blocking accept")
                  (tcp-accept listener)
                  (displayln "Updating event interest")
                  (modify-event-interest-read! poller listener key)))
              events)))

(define (event-loop)
  (displayln "Waiting for events.")
  (events-clear!)
  ;; Socket is ready to go
  (poller-wait poller)
  (displayln "Found events!")

  (let ([events (events->list)])
    (for-each (lambda (event)
                (when (= event key)
                  (displayln "Performing a non blocking accept")
                  (tcp-accept listener)
                  (displayln "Updating event interest")
                  (modify-event-interest-read! poller listener key)))
              events))

  (event-loop))

;; Nonblocking async kind. Signal to the runtime to register
;; an event for this thing.
(define (read-byte output-port)
  (define byte (#%prim.read-byte output-port))
  (if (would-block-object?)
      (begin
        ;; Yield... but schedule this thread to be woken up?
        (async-yield)
        (read-byte output-port))
      byte))

;; Would block -> should immediately yield to the executor when its happening.

;; Tasks, versus not tasks?
(spawn (lambda ()
         (displayln "x foo bar")
         (yield)
         (displayln "bananas")))

(spawn (lambda ()
         (displayln "other task")
         (yield)
         (displayln "foo-bar-baz")))

;; Run the threads?
(start-threads)
