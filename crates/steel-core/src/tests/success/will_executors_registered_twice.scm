;; A value registered more than once is ready once nothing but its
;; registrations refers to it, and each of its wills is run.
(struct frozen (payload label))

(define ran (box '()))
(define executor (make-will-executor))
(define elsewhere (make-will-executor))
(define (note! label)
  (lambda (v) (set-box! ran (cons label (unbox ran)))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))

(define (register-twice make)
  (let ([v (make)])
    (will-register executor v (note! 'first))
    (will-register executor v (note! 'second)))
  #t)

;; Runs the two wills, which have to be there, in the order they were
;; registered.
(define (both-run)
  (collect 6)
  (will-execute executor)
  (will-execute executor)
  (assert! (equal? (unbox ran) '(second first)))
  (set-box! ran '())
  #t)

(register-twice (lambda () (list 1 2 3)))
(both-run)
(register-twice (lambda () (frozen 1 'struct)))
(both-run)
(register-twice (lambda () (string-append "a" "b")))
(both-run)
(register-twice (lambda () (hash 'a 1)))
(both-run)
(register-twice (lambda ()
                  (let ([captured (list 1)])
                    (lambda () captured))))
(both-run)
(register-twice (lambda () (box 1)))
(both-run)
(register-twice (lambda () (mutable-vector 1 2)))
(both-run)

;; In a cycle of its own.
(register-twice (lambda ()
                  (let* ([cell (box #f)]
                         [s (frozen cell 'cycle)])
                    (set-box! cell s)
                    s)))
(both-run)

;; With two executors.
(define (register-with-both)
  (let ([v (frozen 1 'both)])
    (will-register executor v (note! 'here))
    (will-register elsewhere v (note! 'there)))
  #t)
(register-with-both)
(collect 6)
(will-execute elsewhere)
(will-execute executor)
(assert! (equal? (unbox ran) '(here there)))
(set-box! ran '())

;; A will that registers its value again. The value is ready a second time
;; once that will has returned.
(define (register-again)
  (will-register executor
                 (frozen 1 'again)
                 (lambda (v)
                   (set-box! ran (cons 'once (unbox ran)))
                   (will-register executor v (note! 'twice))))
  #t)
(register-again)
(collect 6)
(will-execute executor)
(assert! (equal? (unbox ran) '(once)))
(collect 6)
(will-execute executor)
(assert! (equal? (unbox ran) '(twice once)))
