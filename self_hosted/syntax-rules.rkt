

(struct SyntaxRules (name syntaxes patterns) #:transparent)

(define (check-first expression symbol)
    (equal? (first expression) symbol))

(define (define-syntax? expression) (check-first expression 'define-syntax))

(define (syntax-rules? lst) (check-first lst 'syntax-rules))

(define (syntax-rules->struct name lst)
    (SyntaxRules name (second lst) (rest (rest lst))))

(define test-syntax-rules 
    (syntax-rules->struct 'or
        '(syntax-rules ()
            [(or) #f]
            [(or x) x]
            [(or x y) (let ([z x])
                        (if z z y))]
            [(or x y ...) (or x (or y ...))])))


;; ----- Matcher -----

(define (var? x)
  (and (symbol? x)
       (starts-with? (symbol->string x) "?")))

(define (ignore? x)
  (equal? x '_))

(define (many? x)
  (and (symbol? x)
       (let ((str (symbol->string x)))
         (and (starts-with? str "?")
              (ends-with? str "...")))))


(define (equal-or-insert hm key value)
  (define existing-value (hash-try-get hm key))
  (if existing-value
      (if (equal? existing-value value)
          hm
          #f)
      (hash-insert hm key value)))

(define (hash-get-or-else hm key value)
    (define found (hash-try-get hm key))
    (if found found value))

(define (collect-until-last-p input collected)
  (if (null? (cdr input))
      (list (car input) collected)
      (collect-until-last-p (cdr input) (cons (car input) collected))))


(define (collect-until-last input)
  (collect-until-last-p input '()))


;; Bindings or #false if there is not a match
(define (match-p pattern input bindings)
  (cond [(and (list? pattern)
              (not (null? pattern))
              (many? (car pattern)))
         (if (null? (cdr pattern))
             (equal-or-insert bindings (car pattern) input)
             (let ((collected (collect-until-last input)))
               (define remainder (car collected))
               (define collected-list (reverse (car (cdr collected))))
               (if (null? (cdr (cdr pattern)))
                   (let ((remainder-bound
                          (equal-or-insert bindings
                                           (car (cdr pattern)) remainder)))
                     (equal-or-insert remainder-bound (car pattern) collected-list))
                   #f)))]
        [(var? pattern) (equal-or-insert bindings pattern input)]
        [(ignore? pattern) bindings]
        [(atom? pattern) (if (equal? pattern input) bindings #f)]
        [(null? pattern) (if (null? input) bindings #f)]
        [(null? input) #f]
        [(and (list? pattern) (not (list? input))) #f]
        [else
         (define remaining (match-p (cdr pattern) (cdr input) bindings))
         (if remaining
             (match-p (car pattern) (car input) remaining)
             #f)]))

(define (match pattern input)
  (match-p pattern input (hash)))


(define (contains? pred? lst)
    (cond [(empty? lst) #f]
          [(pred? (car lst)) #t]
          [else (contains? pred? (cdr lst))]))


(define (replace-bindings bindings expression)
    (cond [(atom? expression) (hash-get-or-else bindings expression expression)]
          [(list? expression) => (map (lambda (x) (replace-bindings bindings x)) expression)]
          [else => (error! "Something went wrong in replace bindings")]))

(define (collect-free-vars pattern syntax)
    (cond [(symbol? pattern) (if (contains? (lambda (x) (equal? x pattern)) syntax) '() (list pattern))]
          [(list? pattern) (transduce pattern (flat-mapping (lambda (x) (collect-free-vars x syntax))) (into-list))]
          [else => (error! "Something went from in collect-free-vars")]))
