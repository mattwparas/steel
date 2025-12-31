(provide make-thread-pool
         lock!
         submit-task
         block-on-task
         task-done?
         task-err
         task
         pmap)

;;@doc
;; Lock the given lock during the duration
;; of the thunk.
(define (lock! lock thunk)
  (let ([lock-guard (lock-acquire! lock)])
    (dynamic-wind (lambda () void) (lambda () (thunk)) (lambda () (lock-release! lock-guard)))))

(struct ThreadPool (task-sender capacity thread-handles))

(struct Task (lock done func-or-result err) #:mutable)

(define *running* 'running)
(define *waiting* 'waiting)
(define *done* 'done)

(define (task func)
  (Task (mutex) *waiting* func #f))

;;@doc
;; Check if the given task is done
(define (task-done? t)
  (eq? (Task-done t) 'done))

;;@doc
;; Get the err object (if any) from the given task
(define task-err Task-err)

(define (inner-map func lst)
  (if (empty? lst)
      '()
      (cons (func (first lst)) (inner-map func (rest lst)))))

;;@doc
;; Create a thread pool with the given capacity
(define (make-thread-pool capacity)
  (stdout-simple-displayln "making thread pool")
  (define channels (channels/new))
  (define sender (channels-sender channels))
  (define receiver (channels-receiver channels))

  (define (listen-for-tasks)
    (define next-task (channel/recv receiver))
    (define func (Task-func-or-result next-task))

    ;; Does this work?
    (with-handler (lambda (err)
                    (set-Task-done! next-task *done*)
                    (set-Task-err! next-task err))
                  ;; Capture exception, if it exists. Store it in the task
                  (lock! (Task-lock next-task)
                         (lambda ()
                           (set-Task-done! next-task *running*)
                           ;; This should be fine, we're updating the task to be finished,
                           ;; so we can check the progress of it
                           (set-Task-func-or-result! next-task (func))
                           (set-Task-done! next-task *done*))))

    (listen-for-tasks))

  (define threads (inner-map (lambda (_) (spawn-native-thread listen-for-tasks)) (range 0 capacity)))

  (stdout-simple-displayln threads)

  ;; Give me back a thread pool to do some work
  (ThreadPool sender
              capacity
              threads))

;;@doc
;; Submit task to the thread pool
(define (submit-task tp func)
  ;; Create the task. We'll update this to done, and replace
  ;; the func with the proper value afterwards
  (define task (Task (mutex) #f func #f))
  (channel/send (ThreadPool-task-sender tp) task)
  task)

(define (try-block task)
  (lock! (Task-lock task) (lambda () (Task-func-or-result task))))

;;@doc
;; Block the current thread on this task until it is finished.
(define (block-on-task task)
  ;; This could be acquiring the lock too quickly, since
  ;; it could reach the thread pool after we've acquired this lock.
  ;; So we should attempt to grab the lock, but if the task has not
  ;; been locked yet, then we won't actually wait on it. We should
  ;; just spin lock until the lock can be acquired, and then
  ;; block on it
  (define (loop task)
    (cond
      [(equal? (Task-done task) *waiting*) (loop task)]
      [(equal? (Task-done task) *running*)
       (try-block task)
       (loop task)]
      [(equal? (Task-done task) *done*)
       (if (Task-err task)
           (Task-err task)
           (Task-func-or-result task))]
      [else (loop task)]))

  (loop task))

(define (pmap func lst tp)
  ;; Convert list into chunks that it can operate on, independently - since the
  ;; list is already stored as a bunch of exponential things in a row, we can
  ;; slice it up into those pieces nicely - for now, we can just assume
  ;; that this is something we _could_ implement, and _should_ implement, but I
  ;; don't feel like going through that exercise right now.
  (define chunks (list-chunks lst))
  (define tasks
    (map (lambda (chunk)
           (submit-task tp
                        (lambda ()
                          ;; Find out where the overhead is coming from
                          (define res (map func chunk))

                          res)))
         chunks))
  ;; Reducing contention... how to do it? Probably need to do some kind of work with
  ;; making sure that the globals don't get locked up - I'm guessing that is where most of
  ;; the wait time here is - if each thread can get its own copies of the values, then
  ;; they don't have to be locked up reading a global.
  (transduce tasks (flat-mapping (lambda (x) (block-on-task x))) (into-list)))
