;;;;;;;;;;;; Native threads ;;;;;;;;;;;;;;;;
(require-builtin steel/time)

(define (lock! lock thunk)
  (dynamic-wind (lambda () (lock-acquire! lock))
                (lambda () (thunk))
                (lambda () (lock-release! lock))))

(struct ThreadPool (task-sender capacity thread-handles))

;; This will be updated when the task is finished
(struct Task (lock done func-or-result err) #:mutable)

(define (make-thread-pool capacity)
  (define channels (channels/new))
  (define sender (channels-sender channels))
  (define receiver (channels-receiver channels))

  (define (listen-for-tasks)
    (define next-task (channel/recv receiver))
    (define func (Task-func-or-result next-task))

    (with-handler (lambda (err)
                    (set-Task-err! next-task err)
                    (set-Task-done! next-task #t))
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

;; Should _probably_ get back some lightweight task object
(define (submit-task tp func)
  ;; Create the task. We'll update this to done, and replace
  ;; the func with the proper value afterwards
  (define task (Task (mutex) #f func #f))
  (channel/send (ThreadPool-task-sender tp) task)
  task)

(define (try-block task)
  (lock! (Task-lock task) (lambda () (Task-func-or-result task))))

(define (block-on-task task)
  ;; This could be acquiring the lock too quickly, since
  ;; it could reach the thread pool after we've acquired this lock.
  ;; So we should attempt to grab the lock, but if the task has not
  ;; been locked yet, then we won't actually wait on it. We should
  ;; just spin lock until the lock can be acquired, and then
  ;; block on it
  (define (loop task)
    (cond
      [(Task-done task) (Task-func-or-result task)]
      [else
       (try-block task)
       (loop task)]))

  (loop task))

;; Create work for the thread-pool

(define tp (make-thread-pool 10))

(define (make-task)
  (submit-task tp
               (lambda ()
                 (time/sleep-ms 1000)
                 10)))

(define all-tasks (map (lambda (_) (make-task)) (range 0 10)))

(define results (map block-on-task all-tasks))

(displayln results)

;; Could do some timing tests - but for now this will work
(assert! (= (sum results) 100))

; (assert! (equal? results 100))
