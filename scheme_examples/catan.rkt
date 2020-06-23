;; (define (test) (define make-account (lambda (balance) (lambda (amt) (begin (set! balance (+ balance amt)) balance)))) (define account1 (make-account 100.00))(account1 -20.00)(account1 -20.00 ))

;; stress test the heap

;; (define (test)
;;   (let ([a 10] [b 20])
;;     (lambda () (+ a b))))



;; (define p1 (Player "Matt" 0 '()))
;; (define p2 (Player "Alex" 0 '()))

;; (displayln (add-card-to-player p1 (Wheat)))

;; (define-syntax ->>
;;   (syntax-rules ()
;;     [(->> a) a]
;;     [(->> a (b c ...)) ((l> b c ...) a)]
;;     [(->> a b c ...) (->> (->> a b) c ...)]))


;; (define-syntax method
;;   (syntax-rules ()
;;     [(method type function)]
;;     )


;; (define (test-method this arg1 arg2 ...)
;;   ...)


;; (define blagh (Test-Struct 1 2 3 4 5))

;; (blagh.test-method arg1 arg2) -> (test-method blagh arg1 arg2)





;; (define-syntax enum
;;   (syntax-rules (|)
;;     [(enum a ...)
;;      (define )]
;;   )

;; (define-type
;;   Value
;;   | (Apple a b c)
;;   | (Orange x y z)
;;   | (Banana q r s)
;;   | (None))

;; (define (Value? v)
;;   (or (Apple? v)
;;       (Orange? v)
;;       (Banana? v)
;;       (None? v)))

;; (define-syntax cond
;;   (syntax-rules (else)
;;     [(cond [else e1 ...])
;;       (begin e1 ...)]
;;     [(cond [e1 e2 ...])
;;       (when e1 e2 ...)]
;;     [(cond [e1 e2 ...] c1 ...)
;;       (if e1
;;           (begin e2 ...)
;;           (cond c1 ...))]))

;; (define-syntax test
;;   (syntax-rules ()
;;     [(test a)
;;      (datum->syntax a)]))


;; (define-syntax gen-pred
;;   (syntax-rules ()
;;     [(gen-pred variant x sym)
;;      ((eval (concat-symbols (quote variant) sym)) x)]))



;; (define-mut x 0)
;; (lambda-mut (x y z) (+ x y z))

;; transform syntax from one thing to another thing


(define/contract (test arg1 arg2)
  (-> even? odd? number?)
  (+ arg1 arg2))


;; TODO fix ordering of macro patterns such that mismatch of pattern doesn't happen
(define-syntax define/contract-helper
  (syntax-rules (->)
    [(define/contract-helper name ()
       (-> argc)
       body ...)
     (begin
       (define res ((lambda () body ...)))
       (unless (argc res)
         (error! (symbol->string 'name)
                 ":"
                 "contract violation on result:"
                 res
                 "violated the contract:"
                 (symbol->string 'argc)))
       res)]
    [(define/contract-helper name
       (arg args ...)
       (-> argc argcs ...)
       body ...)
     (begin
       (unless (argc arg)
         (error! (symbol->string 'name)
                 ":"
                 "contract violation:"
                 arg
                 "violated the contract:"
                 (symbol->string 'argc)))
       (define/contract-helper name (args ...) (-> argcs ...) body ...))]))

(define-syntax define/contract
  (syntax-rules (->)
    [(define/contract (name arg args ...)
       (-> argc argcs ...)
       body ...)
     (define (name arg args ...)
       (define/contract-helper name (arg args ...) (-> argc argcs ...) body ...))]))


(define-syntax def-method
  (syntax-rules ()
    [(def-method struct-name (define (a this b ...) body ...))
     (define ((datum->syntax struct-name . a) this b ...)
       (unless ((datum->syntax struct-name ?) this)
         (error! (datum->syntax struct-name . a) "method takes a value of" struct-name "given" this))
       body ...)]))


(define-syntax impl
  (syntax-rules ()
    [(impl struct-name (define (a this b ...) body ...) c ...)
     (begin (def-method struct-name (define (a this b ...) body ...))
            (impl struct-name c ...))]
    [(impl struct-name (define (a this b ...) body ...))
     (def-method struct-name (define (a this b ...) body ...))]))


;; (struct Apple (a b c))
;; (impl Apple
;;       (define (eat this number)
;;         (+ number 25))
;;       (define (color this number)
;;         (+ number 50)))


;; (define-syntax interface)


;; (match Option expr =>
;;        None => (expression)
;;        Some(x) => (expression))

;; (define-type Option
;;   (None ())
;;   (Some (x)))




(define-syntax define-type-help
  (syntax-rules ()
    [(define-type name (variant fields) rest ...)
     (begin
       (struct variant fields)
       (define-type name rest ...))]
    [(define-type name (variant fields))
     (struct variant fields)]))

(define-syntax define-type-body
  (syntax-rules ()
    [(define-type-body x (variant fields) rest ...)
     (or ((datum->syntax variant ?) x)
         (define-type-body x rest ...))]
    [(define-type-body x (variant fields))
     ((datum->syntax variant ?) x)]))


(define-syntax define-type
  (syntax-rules ()
    [(define-type name (variant fields) rest ...)
     (begin
       (define ((datum->syntax name ?) x)
         (define-type-body x (variant fields) rest ...))
       (define-type-help name (variant fields) rest ...))]
    [(define-type-test name (variant fields))
     (begin
       (define ((datum->syntax name ?) x)
         (define-type-body x (variant fields)))
       (define-type-help name (variant fields)))]))


;; (define-syntax define-type
;;   (syntax-rules ()
;;     [(define-type name (a b ...))
;;      (begin
;;        ;; (define-type name (a))
;;        (struct a)
;;        (define-type name (b ...)))]
;;     [(define-type name (a))
;;      (struct a)]))


;; [players]
(struct GameState (players))

(struct Player (name score resources))

;; ADTs

(define (add-card-to-player player resource)
    (set-Player-resources! player
                           (cons
                            resource
                            (Player-resources player))))

(define (update-score-for-player player)
  (define p-resources (Player-resources player))
  (if (> (length (filter (lambda (x) (Wheat? x)) p-resources)) 0)
      (set-Player-score! player
                         (add1 (Player-score player)))
      player))

;; (struct Wheat ())
;; (struct Sheep ())
;; (struct Rock ())



;;
;;           [global env]
;;          /           \
;;       [sub1]        [sub2] -> [sub3] -> [sub4] -> [sub5] ...
;;                        |         |         |         |
;;
;;
;;




(define (guessing-game n)
  (define number (random-int n))
  (displayln "Guess a number between 0 and 20!")
  (define found #f)
  (define (loop)
    (define guessed-number
      (string->int (read-to-string)))
    (if (= guessed-number number)
        (displayln "You found it!")
        (begin
          (if (> guessed-number number)
            (displayln "lower")
            (displayln "higher"))
          (loop))))
  (loop))



(define (main)
  (define finished? #f)
  (define p1 (Player "Matt" 0 '()))
  (define p2 (Player "Alex" 0 '()))
  (define finished #f)

  (while (= finished #f) do
         (set! p1 (add-card-to-player p1 (Wheat)))
         (set! p1 (update-score-for-player p1))
         (when (= (Player-score p1) 10)
           (set! finished #t)
           (displayln "Game ended!")))
  (displayln p1))

(main)
