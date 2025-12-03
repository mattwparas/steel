; (require (prefix-in bar. "bar.scm"))

; bar.bar

; (define (foo)
;   (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

; (set! foo foo)

; (#%jit-compile-2 foo)

; (displayln (foo))

; (define (some? function list)
;   (and (pair? list) (or (function (car list)) (some? function (cdr list)))))

; (set! some? some?)

; (define (map1 func accum lst)
;   (if (null? lst) (reverse accum) (map1 func (cons (func (car lst)) accum) (cdr lst))))

; (set! map1 map1)

; (define (map-many func accum lsts)
;   (if (some? null? lsts)
;       (reverse accum)
;       (map-many func (cons (apply func (map1 car '() lsts)) accum) (map1 cdr '() lsts))))

; (set! map-many map-many)

; (define (map function list1 . more-lists)
;   (if (null? more-lists)
;       (map1 function '() list1)
;       (let ([lists (cons list1 more-lists)])
;         (if (some? null? lists) '() (map-many function '() lists)))))

; (provide map
;          some?
;          map1
;          map-many)

; (#%jit-compile-2 some?)
; (#%jit-compile-2 map1)
; (#%jit-compile-2 map-many)

(require-builtin steel/base)
(require "#%private/steel/control"
         (for-syntax "#%private/steel/control"))

(define (ormap pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (ormap pred (cdr lst))]))

(define (emit-pair printer pair)
  (simple-display "(")
  (let loop ([pair pair])
    (printer (car pair))
    (if (pair? (cdr pair))
        (begin
          (simple-display " ")
          (loop (cdr pair)))
        (begin
          (simple-display " . ")
          (printer (cdr pair))
          (simple-display ")")))))

(define (print-impl obj)
  (define cycle-collector (#%private-cycle-collector obj))

  (for-each (λ (obj)
              (when (int? (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "#")
                (simple-display (#%private-cycle-collector-get cycle-collector obj))
                (simple-display "=")
                (#%top-level-print obj cycle-collector)
                (newline)))
            (#%private-cycle-collector-values cycle-collector))

  (#%top-level-print obj cycle-collector))

(define (#%print obj collector)
  (cond
    [(symbol? obj)
     (let* ([sym (symbol->string obj)]
            [lst (string->list sym)])
       (if (ormap char-whitespace? lst)
           (begin
             (simple-display "|")
             (for-each (λ (x)
                         (when (char=? x #\|)
                           (simple-display "\\"))
                         (simple-display x))
                       lst)
             (simple-display "|"))
           (simple-display sym)))]
    [(char? obj) (write obj)]
    [(string? obj) (write obj)]
    [(atom? obj) (simple-display obj)]
    [(function? obj) (simple-display obj)]
    [(void? obj) (simple-display obj)]
    ;; There is a cycle!
    [(int? (#%private-cycle-collector-get collector obj))
     (simple-display "#")
     (simple-display (#%private-cycle-collector-get collector obj))
     (simple-display "#")]
    [(list? obj)
     (simple-display "(")
     (when (not (empty? obj))
       (#%print (car obj) collector)
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 (cdr obj)))
     (simple-display ")")]
    [(pair? obj) (emit-pair (λ (x) (#%print x collector)) obj)]
    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "#(")
       (when (not (empty? list-obj))
         (#%print (car list-obj) collector)
         (for-each (λ (x)
                     (simple-display " ")
                     (#%print x collector))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(set? obj)
     (let ([list-obj (hashset->list obj)])
       (simple-display "(set")
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 list-obj)
       (simple-display ")"))]
    [(hash? obj)
     (let ([list-obj (transduce obj (into-list))])
       (simple-display "#hash(")
       (when (not (empty? list-obj))
         (simple-display "(")
         (#%print (caar list-obj) collector)
         (simple-display " . ")
         (#%print (cadar list-obj) collector)
         (simple-display ")")
         (for-each (λ (obj)
                     (simple-display " (")
                     (#%print (car obj) collector)
                     (simple-display " . ")
                     (#%print (cadr obj) collector)
                     (simple-display ")"))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(#%private-struct? obj)
     (let ([printer (#%struct-property-ref obj '#:printer)])
       (if (function? printer) (printer obj (λ (x) (#%print x collector))) (simple-display obj)))]
    [else (simple-display obj)]))

(define (#%top-level-print obj collector)
  (cond
    [(symbol? obj)
     (simple-display "'")
     (let* ([sym (symbol->string obj)]
            [lst (string->list sym)])
       (if (ormap char-whitespace? lst)
           (begin
             (simple-display "|")
             (for-each (λ (x)
                         (when (char=? x #\|)
                           (simple-display "\\"))
                         (simple-display x))
                       lst)
             (simple-display "|"))
           (simple-display sym)))]
    [(char? obj) (write obj)]
    [(string? obj) (write obj)]
    [(atom? obj) (simple-display obj)]
    [(list? obj)
     (simple-display "'(")
     (when (not (empty? obj))
       (#%print (car obj) collector)
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 (cdr obj)))
     (simple-display ")")]
    [(pair? obj)
     (simple-display "'")
     (emit-pair (λ (x) (#%print x collector)) obj)]
    [(vector? obj)
     (let ([list-obj (vector->list obj)])
       (simple-display "'#(")
       (when (not (empty? list-obj))
         (#%print (car list-obj) collector)
         (for-each (λ (x)
                     (simple-display " ")
                     (#%print x collector))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(set? obj)
     (let ([list-obj (hashset->list obj)])
       (simple-display "(set")
       (for-each (λ (x)
                   (simple-display " ")
                   (#%print x collector))
                 list-obj)
       (simple-display ")"))]
    [(hash? obj)
     (let ([list-obj (transduce obj (into-list))])
       (simple-display "'#hash(")
       (when (not (empty? list-obj))
         (simple-display "(")
         (#%print (caar list-obj) collector)
         (simple-display " . ")
         (#%print (cadar list-obj) collector)
         (simple-display ")")
         (for-each (λ (obj)
                     (simple-display " (")
                     (#%print (car obj) collector)
                     (simple-display " . ")
                     (#%print (cadr obj) collector)
                     (simple-display ")"))
                   (cdr list-obj)))
       (simple-display ")"))]
    [(#%private-struct? obj)
     (let ([printer (#%struct-property-ref obj '#:printer)])
       (if (function? printer) (printer obj (λ (x) (#%print x collector))) (simple-display obj)))]
    [else (simple-display obj)]))
