(require-builtin steel/time)
(require "steel/result")

(define (loop thread-id)
  (displayln "Hello world!")
  (time/sleep-ms 500)
  (displayln "Hello world from: " thread-id)
  (loop thread-id))

(define (run-background-loop)
  (displayln "Hello world!")
  ; (displayln spawn-thread!)
  (let ([handle (spawn-thread! (λ () (loop 1)))]
        [handle2 (spawn-thread! (λ ()
                                  (time/sleep-ms 250)
                                  (loop 2)))])
    (thread-join! handle2)))

; (spawn-thread! run-background-loop)

; (define (interesting-function)
;   (let ((a (hash 'a 100)))
;     (spawn-thread! (lambda () (time/sleep-ms 1000) (displayln a)))))

; (thread-join! (interesting-function))

;; Channels! Create a channel, send values over the channel, consume on the other one, done.

; (let ((handle (spawn-thread! (λ ()
;                   (+ 10 (list-ref (list "hello world") 0))))))
;   (time/sleep-ms 5000)
;   (unwrap-ok
;     (thread-join! handle)))

;; Spawn thread! -> This requires some up front overhead, however spawning a new thread
;; takes about 400 microseconds, which isn't ideal. In theory we do not need to deep clone _everything_,
;; but there are certain optimizations that need to be implemented up front for this to work.
; (thread-join!
; (spawn-thread! run-background-loop))

;; Spawns a thread, returning a handle to the sender to that thread.
(define (message-passing)
  (define channels (make-channels))
  (define sender (list-ref channels 0))
  (define receiver (list-ref channels 1))

  ;; Worker thread, listen to requests
  (spawn-thread! (lambda ()
                   ;; Process incoming requests.
                   (while #true (displayln (channel->recv receiver)))
                   (loop)))

  sender)

(define *CHILD_THREADS* '())

;; Keep track of all of the threads currently running
(define (record-thread-handle handle)
  (set! *CHILD_THREADS* (cons handle *CHILD_THREADS*)))

(struct CancellableThreadHandle (sender handle))

;;@doc
;; Spawn a function, func, that runs on a background thread, running at the interval `delay-ms`
(define (spawn-cancellable-thread-looping func delay-ms)
  ;; Create the channels. We're going to cancel the thread using
  ;; the sender here to interrupt the receiver
  (define channels (make-channels))
  (define sender (list-ref channels 0))
  (define receiver (list-ref channels 1))

  (CancellableThreadHandle sender
                           (spawn-thread! (lambda ()
                                            (while (not (~> (channel->try-recv receiver) (unwrap-ok)))
                                                   (begin
                                                     (func)
                                                     (time/sleep-ms delay-ms)))
                                            (displayln "Shutting down thread: "
                                                       (thread::current/id))))))

; (define tasks
;   (map
;    (lambda (_) (spawn-cancellable-thread-looping (lambda () (displayln (thread::current/id))) 1000))
;    (range 0 100)))

; (thread-join! (CancellableThreadHandle-handle (car tasks)))

; (let ([cancellable-handler
;        (spawn-cancellable-thread-looping (lambda () (displayln "Hello world!")) 500)])
;   (time/sleep-ms 3000)
;   ;; Cancel the background thread with the interrupt token
;   (channel->send (CancellableThreadHandle-sender cancellable-handler) #t)

;   (displayln "Doing more work after the function!")
;   (time/sleep-ms 1000)
;   (displayln "Finished"))

; (let ([sender (message-passing)])
;   (channel->send sender "Hello world!")
;   (time/sleep-ms 500)
;   (channel->send sender "Second message!")
;   (time/sleep-ms 500)
;   (channel->send sender "and we're done")
;   (time/sleep-ms 500))

; (let ([tasks (map (lambda (_)
;                     (spawn-thread! (lambda ()
;                                      (time/sleep-ms 2000)
;                                      (displayln (thread::current/id)))))
;                   (range 0 10))])
;   (map thread-join! tasks))

(require-builtin steel/process)

; (define (port-stream)
;   (let ((head (read-line-from-port my-port)))
;     (if (equal? 'eof head)
;         empty-stream
;         (stream-cons head (lambda () (port-stream))))))

(define (pipe-stdout channel port)
  (let ([head (read-line-from-port port)])
    (displayln head)
    (if (equal? 'eof head)
        'eof
        (begin
          (channel->send channel head)
          (pipe-stdout channel port)))))

(define (with-stdout-piped command)
  (set-piped-stdout! command)
  command)

(require "steel/result")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN EXPERIMENT WITH LIVE PRINTING SUBPROCESS STUFF
; (define (message-passing-sub-process)
;   (define channels (make-channels))
;   (define sender (list-ref channels 0))
;   (define receiver (list-ref channels 1))

;   ;; Worker thread, listen to requests
;   (record-thread-handle (spawn-thread! (lambda ()
;                                          (define child-process
;                                            ; (command "cargo" '("run" "--" "sleep.scm"))
;                                            (~> (command "cargo" '("run" "--" "sleep.scm"))
;                                                (with-stdout-piped)
;                                                (spawn-process)
;                                                (Ok->value)))

;                                          (define child-stdout (child-stdout child-process))

;                                          ;; Send stuff along the channel until there is no more stuff
;                                          ;; to send
;                                          (pipe-stdout sender child-stdout)

;                                          (wait child-process)

;                                          (channel->send sender 'eof)

;                                          ; (wait child-process)
;                                          )))

;   channels)

; (require "steel/result")

; (define (read-loop receiver)
;   (let ([message (channel->try-recv receiver)])
;     (cond
;       [(Ok? message)
;        (let ([inner-message (unwrap-ok message)])
;          ;; #false would mean we don't have anything to read
;          ;; on the channel, but the thread is not yet finished.
;          (when (string? inner-message)
;            (display inner-message))

;          (unless (equal? 'eof inner-message)

;            (time/sleep-ms 7)

;            (read-loop receiver)))

;        ; (if (thread-finished? (car *CHILD_THREADS*))
;        ; (read-loop receiver)

;        ; (displayln "finished")

;        ; (read-loop receiver))
;        ]
;       [else (displayln "finished")])))

; (define keep-alive #f)

; (define (main)

;   (define sender-and-receiver (message-passing-sub-process))

;   ;; If this gets dropped, it is game over
;   (define sender (list-ref sender-and-receiver 0))
;   (define receiver (list-ref sender-and-receiver 1))

;   (read-loop receiver)

;   (displayln "finished"))

; (main)

;; END EXPERIMENT WITH LIVE PRINTING STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (while (not (~> (channel->try-recv receiver) (unwrap-ok)))
;        (begin
;          (displayln)
;          (time/sleep-ms delay-ms))))

(define (message-passing-sub-process)
  (define channels (make-channels))
  (define sender (list-ref channels 0))
  (define receiver (list-ref channels 1))

  (define command-channels (make-channels))
  (define command-sender (list-ref channels 0))
  (define command-receiver (list-ref channels 1))

  ;; Worker thread, listen to requests
  (record-thread-handle
   (spawn-thread! (lambda ()
                    (define child-process
                      ; (command "cargo" '("run" "--" "sleep.scm"))
                      (~> (command "bash" '()) (with-stdout-piped) (spawn-process) (Ok->value)))

                    (define child-stdout (child-stdout child-process))
                    (define child-stdin (child-stdin child-process))

                    (write-line! child-stdin "ls")

                    ;; Send stuff along the channel until there is no more stuff
                    ;; to send
                    (pipe-stdout sender child-stdout)

                    ; (channel->send sender 'eof)

                    (write-line! child-stdin "ls")

                    (pipe-stdout sender child-stdout)

                    (channel->send sender 'eof)

                    ; (pipe-stdout sender child-stdout)

                    ; (wait child-process)

                    ; (channel->send sender 'eof)

                    ; (wait child-process)
                    )))

  channels)

(define (read-loop receiver)
  (let ([message (channel->try-recv receiver)])
    (cond
      [(Ok? message)
       (let ([inner-message (unwrap-ok message)])
         ;; #false would mean we don't have anything to read
         ;; on the channel, but the thread is not yet finished.
         (when (string? inner-message)
           (display inner-message))

         (unless (equal? 'eof inner-message)

           (time/sleep-ms 7)

           (read-loop receiver)))

       ; (if (thread-finished? (car *CHILD_THREADS*))
       ; (read-loop receiver)

       ; (displayln "finished")

       ; (read-loop receiver))
       ]
      [else (displayln "finished")])))

(define keep-alive #f)

(define (main)

  (define sender-and-receiver (message-passing-sub-process))

  ;; If this gets dropped, it is game over
  (define sender (list-ref sender-and-receiver 0))
  (define receiver (list-ref sender-and-receiver 1))

  (read-loop receiver)

  (thread-join! (car *CHILD_THREADS*))

  (displayln "finished"))

(main)
