;; A registered value in a list that is in use is not ready, even when the
;; only other thing showing it is a tail of that list held by something that
;; is not in use. Lists share what they hold with their tails.
(struct frozen (payload label))

(define executor (make-will-executor))
(define ran (box '()))
(define (note! label)
  (lambda (v) (set-box! ran (cons label (unbox ran)))))
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(define (next-ready)
  (collect 6)
  (will-execute executor)
  (car (unbox ran)))

(define in-use (box #f))
(define (let-go!)
  (set-box! in-use #f)
  #t)

;; Registers the second element of a list that is in use, and then a value
;; nothing holds, which holds the tail of the list starting there.
(define (register-second make)
  (let ([elements (list 'zero (make) 'two)])
    (set-box! in-use elements)
    (will-register executor (cadr elements) (note! 'element))
    (will-register executor (frozen (cdr elements) 'tail) (note! 'holds-tail)))
  #t)

(define (check make)
  (set-box! ran '())
  (register-second make)
  (assert! (eq? (next-ready) 'holds-tail))
  ;; Still not ready with the tail gone.
  (will-register executor (list 'unheld) (note! 'unheld))
  (assert! (eq? (next-ready) 'unheld))
  (assert! (= (length (unbox in-use)) 3))
  (let-go!)
  (assert! (eq? (next-ready) 'element))
  #t)

(check (lambda () (frozen 1 'struct)))
(check (lambda () (list 1 2)))
(check (lambda () (string-append "o" "ne")))
(check (lambda () (hash 'a 1)))
(check (lambda () (box 1)))

;; The same with a hash map made from one that is in use.
(define (register-in-map make)
  (let* ([v (make)]
         [m (hash 'key v)])
    (set-box! in-use m)
    (will-register executor v (note! 'element))
    (will-register executor (frozen (hash-insert m 'other 1) 'tail) (note! 'holds-tail)))
  #t)

(define (check-map make)
  (set-box! ran '())
  (register-in-map make)
  (assert! (eq? (next-ready) 'holds-tail))
  (will-register executor (list 'unheld) (note! 'unheld))
  (assert! (eq? (next-ready) 'unheld))
  (let-go!)
  (assert! (eq? (next-ready) 'element))
  #t)

(check-map (lambda () (frozen 1 'struct)))
(check-map (lambda () (string-append "o" "ne")))
