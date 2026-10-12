(require-builtin steel/time)

(define p (make-parameter 'default))
(define q (make-parameter 'q))

(parameterize ([p 1] [q 2])
  (assert! (equal? (list (p) (q)) '(1 2)))
  (parameterize ([p 3])
    (assert! (equal? (p) 3))
    (p 30)
    (assert! (equal? (p) 30)))
  (assert! (equal? (p) 1)))
(assert! (equal? (list (p) (q)) '(default q)))

(assert! (equal? (with-handler (lambda (e) (p)) (parameterize ([p 1]) (error "raised"))) 'default))

;; Threads bound to different values at once
(define (bound-for name ms)
  (spawn-native-thread (lambda ()
                         (parameterize ([p name])
                           (time/sleep-ms ms)
                           (p)))))

(define threads (map (lambda (name) (bound-for name 100)) '(a b c d)))
(time/sleep-ms 30)
(assert! (equal? (p) 'default))
(assert! (equal? (map thread-join! threads) '(a b c d)))
(assert! (equal? (p) 'default))

;; A thread starts with its spawner's bindings
(define child
  (parameterize ([p 'parent])
    (spawn-native-thread (lambda ()
                           (let ([inherited (p)])
                             (p 'changed)
                             (list inherited (p) (parameterize ([p 'child]) (p))))))))
(assert! (equal? (thread-join! child) '(parent changed child)))
(assert! (equal? (p) 'default))

;; Without a binding, assigning sets the default for every thread
(p 'assigned)
(assert! (equal? (thread-join! (spawn-native-thread (lambda () (p)))) 'assigned))

;; Output captured on one thread while another prints
(define printer
  (spawn-native-thread (lambda ()
                         (parameterize ([current-output-port (open-output-string)])
                           (time/sleep-ms 100)
                           (display "elsewhere")))))
(define captured
  (call-with-output-string (lambda (port)
                             (parameterize ([current-output-port port])
                               (time/sleep-ms 150)
                               (display "here")))))
(thread-join! printer)
(assert! (equal? captured "here"))
