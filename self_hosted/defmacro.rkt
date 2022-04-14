;; TODO: write functions that extract defmacros, turn them into functions that operate on syntax by rewriting them
;; Evaluate them in a closed world vm - emit back the code that they generate. Run this step in the compilation process.

;; Accepts a defmacro expression and emits a define expression
(define/contract (rewrite-defmacro define-expr)
  (->/c list? list?)
  (cons 'define (cdr define-expr)))

;; Check if the given value is in fact a def macro expression
(define (defmacro? expression)
  (and (list? expression) (equal? 'defmacro (car expression))))

;; Returns the name of the defmacro to assist with calling
(define (extract-macro-name defmacro)
  (car (cdr defmacro)))


;; Split a list into two distinct versions based on the predicate
;; The left list contains all true values
(define (partition-list lst predicate)
  (->/c list? (listof list?))
  (define (accumulate lst left right predicate)
    (cond [(empty? lst) => (list (reverse left) (reverse right))]
          [(predicate (car lst)) => (accumulate (cdr lst) (cons (car lst) left) right predicate)]
          [else => (accumulate (cdr lst) left (cons (car lst) right) predicate)]))
    (accumulate lst '() '() predicate))


(displayln (rewrite-defmacro '(defmacro identity (lambda (x) x))))

(define program '((defmacro identity (lambda (x) x))
                  (defmacro other-identity (lambda (x) x))
                  (defmacro repeat-to-list (lambda (x) (list x x x x x x)))
                  (defmacro make-struct (lambda (struct-name fields)
                                          (cons 'begin
                                                (map (lambda (field)
                                                       (list 'define
                                                             (concat-symbols struct-name '- (car field))
                                                             (list 'lambda
                                                                   '(this)
                                                                   (list 'vector-ref 'this (car (cdr field))))))
                                               (enumerate 0 '() fields)))))

                  (identity 10)
                  (identity 20)
                  (other-identity 30)
                  (define (identity-function x) x)))

(define helpers '(
                   (define (enumerate start accum lst)
                    (if (empty? lst)
                        (reverse accum)
                        (enumerate (+ start 1)
                                   (cons (list (car lst) start)
                                         accum)
                                   (cdr lst))))

                   ))


;; This is going to be our engine
(define *engine* (Engine::new))
(run! *engine* helpers)


(define partitioned-program (partition-list program defmacro?))

;; corresponds to our macro definitions
(define defmacro-list (map rewrite-defmacro (list-ref partitioned-program 0)))
;; The rest of the program excluding the macros
(define program-list (list-ref partitioned-program 1))
;; The set of available macros
(define defmacro-name-set (transduce defmacro-list
                                     (mapping extract-macro-name)
                                     (into-hashset)))

;; Registers the functions in our engine
;; In order to eval a macro expansion, we just have to call the function and extract the output
(run! *engine* defmacro-list)


;; Goes into the environment, and calls the given symbol with the arguments passed in as a list
;; This is a bit spooky, but this effectively gives us an evaluation environment for macros
(call-function-in-env *engine* 'identity (list
                                          '(define x 10)))


(define (expand-macros expression macro-set engine)
  (cond [(symbol? expression) => expression]
        [(empty? expression) => expression]
        [(list? expression) =>
                            (if (hashset-contains? macro-set (car expression))
                                (begin
                                  (displayln (cdr expression))
                                  (expand-macros
                                   (call-function-in-env engine (car expression) (cdr expression))
                                   macro-set
                                   engine))
                                (map (lambda (expr) (expand-macros expr macro-set engine)) expression))]
        [else => expression]))


;; (expand-macros '(make-struct Applesauce (a b c)) defmacro-name-set *engine*)
