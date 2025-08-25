;;; LATTICE -- Obtained from Andrew Wright.

; (import (scheme base)
;         (scheme read)
;         (scheme write)
;         (scheme time))

(require "common.scm")

(define xreverse! reverse)

; (define (map-inner func accum lst)
;   (if (empty? lst)
;       (reverse accum)
;       (map-inner func (cons (func (car lst)) accum) (cdr lst))))

; (define (map func lst)
;   (map-inner func '() lst))

(define (iterator-map iter func accum)
  (define next-value (iter-next! iter))
  (if (eq? next-value #%iterator-finished)
      (reverse accum)
      (iterator-map iter func (cons (func next-value) accum))))

(define (map func lst)
  (define iterator (value->iterator lst))
  ;; Walk the list, and do something with it
  (iterator-map iterator func '()))

;; Given a comparison routine that returns one of
;;       less
;;       more
;;       equal
;;       uncomparable
;; return a new comparison routine that applies to sequences.
(define lexico
  (lambda (base)
    (define lex-fixed
      (lambda (fixed lhs rhs)
        (define check
          (lambda (lhs rhs)
            (if (null? lhs)
                fixed
                (let ([probe (base (car lhs) (car rhs))])
                  (if (or (eq? probe 'equal) (eq? probe fixed))
                      (check (cdr lhs) (cdr rhs))
                      'uncomparable)))))
        (check lhs rhs)))
    (define lex-first
      (lambda (lhs rhs)
        (if (null? lhs)
            'equal
            (let ([probe (base (car lhs) (car rhs))])
              (case probe
                [(less more) (lex-fixed probe (cdr lhs) (cdr rhs))]
                [(equal) (lex-first (cdr lhs) (cdr rhs))]
                [(uncomparable) 'uncomparable])))))
    lex-first))

(define (make-lattice elem-list cmp-func)
  (cons elem-list cmp-func))

;; TODO: Figure out how to inline when its a static definition
(define lattice->elements car)

(define lattice->cmp cdr)

;; Select elements of a list which pass some test.
(define zulu-select
  (lambda (test lst)
    (define select-a
      (lambda (ac lst)
        (if (null? lst)
            (xreverse! ac)
            (select-a (let ([head (car lst)])
                        (if (test head)
                            (cons head ac)
                            ac))
                      (cdr lst)))))
    (select-a '() lst)))

; (define xreverse!
;   (letrec ([rotate (lambda (fo fum)
;                      (let ([next (cdr fo)])
;                        (set-cdr! fo fum)
;                        (if (null? next) fo (rotate next fo))))])
;     (lambda (lst) (if (null? lst) '() (rotate lst '())))))

;; Select elements of a list which pass some test and map a function
;; over the result.  Note, only efficiency prevents this from being the
;; composition of select and map.
(define select-map
  (lambda (test func lst)
    (define select-a
      (lambda (ac lst)
        (if (null? lst)
            (xreverse! ac)
            (select-a (let ([head (car lst)])
                        (if (test head)
                            (cons (func head) ac)
                            ac))
                      (cdr lst)))))
    (select-a '() lst)))

;; This version of map-and tail-recurses on the last test.
(define map-and
  (lambda (proc lst)
    (if (null? lst)
        #t
        (letrec ([drudge
                  (lambda (lst)
                    ; (let ([rest (cdr lst)])
                    ;   (if (null? rest) (proc (car lst)) (and (proc (car lst)) (drudge rest)))))])
                    (if (cdr-null? lst)
                        (proc (car lst))
                        (and (proc (car lst)) (drudge (cdr lst)))))])
          (drudge lst)))))

(define (maps-1 source target pas new)
  ; (let ([scmp (lattice->cmp source)]
  ;       [tcmp (lattice->cmp target)])
  (let ([scmp (cdr source)]
        [tcmp (cdr target)])
    (let ([less (select-map (lambda (p) (eq? 'less (scmp (car p) new))) cdr pas)]
          [more (select-map (lambda (p) (eq? 'more (scmp (car p) new))) cdr pas)])
      (zulu-select (lambda (t)
                     (and (map-and (lambda (t2) (memq (tcmp t2 t) '(less equal))) less)
                          (map-and (lambda (t2) (memq (tcmp t2 t) '(more equal))) more)))
                   ; (lattice->elements target)))))
                   (car target)))))

(define (maps-rest source target pas rest to-1 to-collect)
  (if (null? rest)
      (to-1 pas)
      (let ([next (car rest)]
            [rest (cdr rest)])
        (to-collect (map (lambda (x)
                           (maps-rest source target (cons (cons next x) pas) rest to-1 to-collect))
                         (maps-1 source target pas next))))))

(define (maps source target)
  (make-lattice (maps-rest source
                           target
                           '()
                           ; (lattice->elements source)
                           (car source)
                           (lambda (x) (list (map cdr x)))
                           (lambda (x) (apply append x)))
                ; (lexico (lattice->cmp target))))
                (lexico (cdr target))))

(define (count-maps source target)
  ; (maps-rest source target '() (lattice->elements source) (lambda (x) 1) sum))
  (maps-rest source target '() (car source) (lambda (x) 1) sum))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (run k)
  (let* ([l2 (make-lattice '(low high)
                           (lambda (lhs rhs)
                             (case lhs
                               [(low)
                                (case rhs
                                  [(low) 'equal]
                                  [(high) 'less]
                                  [else (error 'make-lattice "base" rhs)])]
                               [(high)
                                (case rhs
                                  [(low) 'more]
                                  [(high) 'equal]
                                  [else (error 'make-lattice "base" rhs)])]
                               [else (error 'make-lattice "base" lhs)])))]
         [l3 (maps l2 l2)]
         [l4 (maps l3 l3)])
    (count-maps l2 l2)
    (count-maps l3 l3)
    (count-maps l2 l3)
    (count-maps l3 l2)
    (case k
      [(33) (count-maps l3 l3)]
      [(44) (count-maps l4 l4)]
      [(45) (let ([l5 (maps l4 l4)]) (count-maps l4 l5))]
      [(54) (let ([l5 (maps l4 l4)]) (count-maps l5 l4))]
      [(55) (let ([l5 (maps l4 l4)]) (count-maps l5 l5))]
      [else (error "run: unanticipated problem size" k)])))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 (number->string input1)]
         [name "lattice"])
    (run-r7rs-benchmark (string-append name ":" s1 ":" s2)
                        count
                        (lambda () (run (hide count input1)))
                        (lambda (result) (= result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/lattice.input" run-benchmark)

; (inspect maps-rest)
