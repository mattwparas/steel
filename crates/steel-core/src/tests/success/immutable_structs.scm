;; Structs whose fields can't be changed keep them inline, in one allocation
;; with the type ahead of them. The loops run often enough that the compiled
;; accessors are the ones checked.
(struct point (x y))
(struct shape (name points) #:transparent)
(struct unit ())
(struct wide (a b c d e f g h i j k l))
(struct callable (f extra) #:prop:procedure 0)
(struct wrapper (inner))
(struct other-point (x y))

(define (raised? thunk)
  (eq? 'raised (with-handler (lambda (e) 'raised) (thunk))))

;; Making one, asking what it is, and reading it.
(define p (point 1 2))
(assert! (point? p))
(assert! (not (point? 5)))
(assert! (not (point? (list 1 2))))
(assert! (not (point? (other-point 1 2))))
(assert! (not (shape? p)))
(assert! (= (point-x p) 1))
(assert! (= (point-y p) 2))

;; No fields, and many.
(define u (unit))
(assert! (unit? u))
(assert! (not (unit? p)))
(define w (wide 1 2 3 4 5 6 7 8 9 10 11 12))
(assert! (wide? w))
(assert! (= (wide-a w) 1))
(assert! (= (wide-f w) 6))
(assert! (= (wide-l w) 12))
(assert! (= (+ (wide-a w) (wide-b w) (wide-c w) (wide-d w) (wide-e w) (wide-f w)
               (wide-g w) (wide-h w) (wide-i w) (wide-j w) (wide-k w) (wide-l w))
            78))

;; A field holds any kind of value, and gives back that value.
(define held (box 'held))
(define everything
  (wide 1 2.5 "three" 'four #\5 (list 6) (mutable-vector 7) held (hash 'nine 9) (lambda () 10) p void))
(assert! (equal? (wide-c everything) "three"))
(assert! (eq? (wide-d everything) 'four))
(assert! (equal? (wide-f everything) (list 6)))
(assert! (eq? (wide-h everything) held))
(assert! (= ((wide-j everything)) 10))
(assert! (eq? (wide-k everything) p))

;; Each is itself and nothing else is.
(assert! (eq? p p))
(assert! (not (eq? (point 1 2) (point 1 2))))
(assert! (equal? p p))
(define same-shape (shape "s" (list (point 0 0))))
(assert! (eq? same-shape same-shape))

;; Ones that show their fields are compared by them.
(assert! (equal? (shape "s" (list 1 2)) (shape "s" (list 1 2))))
(assert! (not (equal? (shape "s" (list 1 2)) (shape "s" (list 1 3)))))
(assert! (not (equal? (shape "s" (list 1 2)) (shape "t" (list 1 2)))))
(assert! (equal? (shape "s" (shape "inner" 1)) (shape "s" (shape "inner" 1))))
(assert! (not (equal? (shape "s" (shape "inner" 1)) (shape "s" (shape "inner" 2)))))
;; One type is never equal to another, whatever they hold.
(assert! (not (equal? (point 1 2) (other-point 1 2))))
(assert! (not (equal? (shape 1 2) (point 1 2))))
(assert! (not (equal? (point 1 2) 5)))
(assert! (not (equal? (unit) (point 1 2))))

;; As keys, and in sets.
(define by-shape (hash (shape "a" 1) 'first (shape "b" 2) 'second))
(assert! (eq? (hash-ref by-shape (shape "a" 1)) 'first))
(assert! (eq? (hash-ref by-shape (shape "b" 2)) 'second))
(assert! (not (hash-contains? by-shape (shape "a" 2))))
(define by-point (hash p 'this-one))
(assert! (eq? (hash-ref by-point p) 'this-one))

;; Inside other values, and holding each other.
(define nested (wrapper (wrapper (wrapper p))))
(assert! (eq? (wrapper-inner (wrapper-inner (wrapper-inner nested))) p))
(define in-list (list (point 1 2) (point 3 4) (point 5 6)))
(assert! (= (apply + (map point-x in-list)) 9))
(assert! (equal? (map point-y in-list) (list 2 4 6)))
(define in-vector (mutable-vector (point 1 2) (point 3 4)))
(assert! (= (point-y (vector-ref in-vector 1)) 4))
(define in-box (box (point 7 8)))
(assert! (= (point-x (unbox in-box)) 7))
(assert! (= (length (filter point? (list 1 p "two" (point 0 0) (other-point 1 1) u))) 2))

;; The wrong type is an error, and not a read of whatever is there.
(assert! (raised? (lambda () (point-x 5))))
(assert! (raised? (lambda () (point-x (list 1 2)))))
(assert! (raised? (lambda () (point-x (other-point 1 2)))))
(assert! (raised? (lambda () (point-y (shape 1 2)))))
(assert! (raised? (lambda () (point-x u))))
(assert! (raised? (lambda () (wide-l p))))
(assert! (raised? (lambda () (wrapper-inner p))))

;; One that can be called runs the function in its field.
(define add (callable (lambda (a b) (+ a b)) 'extra))
(assert! (= (add 1 2) 3))
(assert! (callable? add))
(assert! (eq? (callable-extra add) 'extra))
(assert! (equal? (map (callable (lambda (x) (* x x)) #f) (list 1 2 3)) (list 1 4 9)))

;; Printing shows the fields of those that show them.
(assert! (equal? (to-string (shape "s" 1)) "(shape \"s\" 1)"))

;; The accessors as values, passed around like any function.
(assert! (equal? (map point-x (list (point 1 0) (point 2 0))) (list 1 2)))
(define (apply-twice f v) (f (f v)))
(assert! (eq? (apply-twice wrapper-inner (wrapper (wrapper 'deep))) 'deep))
(define make (list point other-point))
(assert! (point? ((car make) 1 2)))
(assert! (other-point? ((cadr make) 1 2)))

;; A struct type made inside a function.
(define (make-local n)
  (struct local (value))
  (let ([made (local n)])
    (and (local? made) (local-value made))))
(assert! (= (make-local 5) 5))
(assert! (= (make-local 6) 6))

;; Hot loops, so the compiled accessors and predicates are what run.
(define (sum-xs points total)
  (if (null? points)
      total
      (sum-xs (cdr points) (+ total (point-x (car points)) (point-y (car points))))))
(define many (map (lambda (n) (point n (* 2 n))) (range 0 2000)))
(assert! (= (sum-xs many 0) (* 3 (apply + (range 0 2000)))))

(define (count-points values n)
  (if (null? values)
      n
      (count-points (cdr values) (if (point? (car values)) (+ n 1) n))))
(define mixed (map (lambda (n) (if (even? n) (point n n) (other-point n n))) (range 0 2000)))
(assert! (= (count-points mixed 0) 1000))

(define (build-and-read n total)
  (if (= n 0)
      total
      (let ([made (point n (wrapper n))])
        (build-and-read (- n 1) (+ total (point-x made) (wrapper-inner (point-y made)))))))
(assert! (= (build-and-read 3000 0) (* 2 (apply + (range 1 3001)))))

(define (walk-wide w n total)
  (if (= n 0)
      total
      (walk-wide w (- n 1) (+ total (wide-a w) (wide-g w) (wide-l w)))))
(assert! (= (walk-wide w 3000 0) (* 3000 20)))

;; The same loop meets the wrong type part way through.
(define (sum-until-wrong values total)
  (if (null? values)
      total
      (sum-until-wrong (cdr values) (+ total (point-x (car values))))))
(define with-a-stranger (append (map (lambda (n) (point n n)) (range 0 500)) (list (other-point 1 1))))
(assert! (raised? (lambda () (sum-until-wrong with-a-stranger 0))))
(assert! (= (sum-until-wrong (map (lambda (n) (point n n)) (range 0 500)) 0) (apply + (range 0 500))))

;; A long chain comes apart without going down the stack a link at a time.
(define (chain n previous)
  (if (= n 0) previous (chain (- n 1) (wrapper previous))))
(define (depth w n)
  (if (wrapper? w) (depth (wrapper-inner w) (+ n 1)) n))
(define (make-and-drop)
  (let ([long (chain 300000 'end)])
    (depth long 0)))
(assert! (= (make-and-drop) 300000))

;; One made on one thread and read on another.
(define made-here (shape "shared" (list (point 1 2) (point 3 4))))
(define reader
  (spawn-native-thread (lambda ()
                         (let loop ([n 0] [total 0])
                           (if (= n 5000)
                               total
                               (loop (+ n 1) (+ total (point-x (cadr (shape-points made-here))))))))))
(assert! (= (thread-join! reader) 15000))
(assert! (equal? (thread-join! (spawn-native-thread (lambda () (shape "made" (list 1 2)))))
                 (shape "made" (list 1 2))))
