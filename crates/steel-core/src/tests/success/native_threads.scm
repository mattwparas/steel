;;;;;;;;;;;; Native threads ;;;;;;;;;;;;;;;;
(require-builtin steel/time)

(define (lock! lock thunk)
  (lock-acquire! lock)
  (dynamic-wind (lambda () void) (lambda () (thunk)) (lambda () (lock-release! lock))))

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

;; Should _probably_ get back some lightweight task object
(define (submit-task tp func)
  ;; Create the task. We'll update this to done, and replace
  ;; the func with the proper value afterwards
  (define task (Task (mutex) #f func #f))
  (channel/send (ThreadPool-task-sender tp) task)
  task)

(define (block-on-task task)
  (lock! (Task-lock task) (lambda () (Task-func-or-result task))))

;; Create work for the thread-pool

(define tp (make-thread-pool 10))

(define (make-task)
  (submit-task tp
               (lambda ()
                 (time/sleep-ms 1000)
                 10)))

(define all-tasks (map (lambda (_) (make-task)) (range 0 10)))

;; Could do some timing tests - but for now this will work
(assert! (= (sum (map block-on-task all-tasks)) (sum (map (lambda (_) 10) (range 0 10)))))
