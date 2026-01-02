(define type-table
  (hash '#%prim.cons
        '((any list?) list?)
        '#%prim.car
        '((list?) any)
        '#%prim.cdr
        '((list?) any)
        '#%prim.+
        '((number? number?) number?)))

(define primitive-table
  (hash '#%prim.list?
        'list?
        '#%prim.number?
        'number?
        '#%prim.int?
        'int?
        '#%prim.integer?
        'int?
        '#%prim.string?
        'string?))

(define (check-type-info raw-expr type-info)
  (match-syntax
   raw-expr
   [`(if (,primitive-type-check ,variable) ,then-expr ,else-expr)
    (define primitive-type-check-type (hash-try-get primitive-table (syntax-e primitive-type-check)))
    (let ([then-expr-type (check-type-info
                           then-expr
                           (hash-insert type-info (syntax-e variable) primitive-type-check-type))]
          [else-expr-type (check-type-info else-expr type-info)])

      (if (equal? then-expr-type else-expr-type)
          then-expr-type
          ;; Just promote to the any type
          'any))]
   ;; If this doesn't match any of our other forms, recur
   ; [(list other ...) (map (lambda (e) (check-type-info e type-info)) other)]
   #;[(list other ...) '()]
   ;; We've bottomed out, just return the collected type information
   ; [other (or (hash-try-get type-info (syntax-e other)) 'any)]))
   [other (or (hash-try-get type-info (syntax-e other)) 'any)]))

; (set! check-type-info check-type-info)

; (define my-expr2
;   (quasisyntax (define loop
;                  (lambda (maybe-list)
;                    (if (#%prim.list? maybe-list) (#%prim.car maybe-list) (+ maybe-list 10))))))

;; type-check...
; (check-type-info my-expr2 type-table)

; (set! check-type-info check-type-info)
