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
  (dynamic-wind (lambda () (lock-acquire! lock))
                (lambda () (thunk))
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
    (with-handler (lambda (err)
                    (set-Task-done! next-task #t)
                    (set-Task-err! next-task err))
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
      ;; If its an error, we don't immediately raise
      ;; the exception for now
      [(Task-done task)
       (if (Task-err task)
           (Task-err task)
           (Task-func-or-result task))]
      [else
       (try-block task)
       (loop task)]))

  (loop task))
