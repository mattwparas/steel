(provide make-thread-pool
         lock!
         submit-task
         block-on-task
         task-done?
         task-err
         task)

;;@doc
;; Lock the given lock during the duration
;; of the thunk.
(define (lock! lock thunk)
  (lock-acquire! lock)
  (dynamic-wind (lambda () void)
                (lambda ()
                  (thunk)
                  (lock-release! lock))
                (lambda () (lock-release! lock))))

(struct ThreadPool (task-sender capacity thread-handles))

(struct Task (lock done func-or-result err) #:mutable)

(define (task func)
  (Task (mutex) #f func #f))

;;@doc
;; Check if the given task is done
(define task-done? Task-done)

;;@doc
;; Get the err object (if any) from the given task
(define task-err Task-err)

;;@doc
;; Create a thread pool with the given capacity
(define (make-thread-pool capacity)
  (define channels (channels/new))
  (define sender (channels-sender channels))
  (define receiver (channels-receiver channels))

  (define (listen-for-tasks)
    (define next-task (channel/recv receiver))
    (define func (Task-func-or-result next-task))

    ;; Does this work?
    (with-handler (lambda (err) (set-Task-err! next-task err))
                  ;; Capture exception, if it exists. Store it in the task
                  (lock! (Task-lock next-task)
                         (lambda ()
                           ;; This should be fine, we're updating the task to be finished,
                           ;; so we can check the progress of it
                           (set-Task-func-or-result! next-task (func))
                           (set-Task-done! next-task #t))))

    (listen-for-tasks))

  ;; Give me back a thread pool to do some work
  (ThreadPool sender
              capacity
              (map (lambda (_) (spawn-native-thread listen-for-tasks)) (range 0 capacity))))

;;@doc
;; Submit task to the thread pool
(define (submit-task tp func)
  ;; Create the task. We'll update this to done, and replace
  ;; the func with the proper value afterwards
  (define task (Task (mutex) #f func #f))
  (channel/send (ThreadPool-task-sender tp) task)
  task)

;;@doc
;; Block the current thread on this task until it is finished.
(define (block-on-task task)
  (lock! (Task-lock task) (lambda () (Task-func-or-result task))))
