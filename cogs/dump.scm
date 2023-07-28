(begin
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*SUCCESS-COUNT*
        0)
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*FAILURE-COUNT*
        0)
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*failures*
        (quote
          ()))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-success
        (λ ()
          (set! mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*SUCCESS-COUNT*
            (+
               mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*SUCCESS-COUNT*
               1))))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
        (λ (name)
          (begin
                (set! mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*FAILURE-COUNT*
                  (+
                     mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*FAILURE-COUNT*
                     1))
                (set! mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*failures*
                  (cons
                     name
                     mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*failures*)))))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-success
        (λ (name)
          (begin
                (display "test > " name " ... ")
                (display-color "Ok" (quote green))
                (newline))))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
        (λ (name)
          (begin
                (display "test > " name " ... ")
                (display-color "FAILED" (quote red))
                (newline))))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
        (λ (name input expected)
          (if (equal? input expected)
            (begin
                  (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-success)
                  (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-success
                     name))
            (begin
                  (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                     name)
                  (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                     name)
                  (displayln
                     "    Expected: "
                     expected
                     ", Found "
                     input)))))
      (define mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmget-test-stats
        (λ ()
          (hash
             (quote
               success-count)
             mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*SUCCESS-COUNT*
             (quote
               failure-count)
             mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*FAILURE-COUNT*
             (quote
               failures)
             mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm*failures*)))
      (define __module-mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm
        (hash
           (quote
             test)
           mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
           (quote
             get-test-stats)
           mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmget-test-stats)))

(define test
  (hash-get
     __module-mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm
     (quote
       test)))

(define get-test-stats
  (hash-get
     __module-mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm
     (quote
       get-test-stats)))

(define test
  (hash-get
     __module-mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm
     (quote
       test)))

(define get-test-stats
  (hash-get
     __module-mangler/home/matt/Documents/steel/cogs/tests/unit-test.scm
     (quote
       get-test-stats)))

(set-test-mode!)

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "addition")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "addition")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "addition"
             8
             (+ x x))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Variable arity function call")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Variable arity function call")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Variable arity function call"
             (quote
               (3 4 5 6))
             x)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Rest arguments")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Rest arguments")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Rest arguments"
             (quote
               (5 6))
             (quote
               (5 6)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Branching with >")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Branching with >")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Branching with >"
             (quote
               yes)
             (quote
               yes))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Branch with <")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Branch with <")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Branch with <"
             (quote
               no)
             (quote
               no))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Numeric operations with if")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Numeric operations with if")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Numeric operations with if"
             1
             1)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Cond with >")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Cond with >")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Cond with >"
             (quote
               greater)
             (quote
               greater))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Cond with equal")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Cond with equal")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Cond with equal"
             (quote
               equal)
             (quote
               equal))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Case macro")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Case macro")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Case macro"
             (quote
               composite)
             (if (member 6 (quote (2 3 5 7)))
               (quote
                 prime)
               (if (member 6 (quote (1 4 6 8 9)))
                 (quote
                   composite)
                 void)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Case with chars")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Case with chars")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Case with chars"
             (quote
               consonant)
             (let ((##atom-key (quote c)))
               (if (member ##atom-key (quote (a e i o u)))
                 (quote
                   vowel)
                 (if (member ##atom-key (quote (w y)))
                   (quote
                     semivowel)
                   (quote
                     consonant)))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "and true")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "and true")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "and true"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "and false")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "and false")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "and false"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "and returns last in the list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "and returns last in the list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "and returns last in the list"
             (quote
               (f g))
             (quote
               (f g)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "and defaults to true")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "and defaults to true")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "and defaults to true"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "or true on the first")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "or true on the first")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "or true on the first"
             #true
             (quote
               #true))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "or true on the first, second not true")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "or true on the first, second not true")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "or true on the first, second not true"
             #true
             (quote
               #true))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "basic let")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "basic let")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "basic let"
             6
             (* x y))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "basic let with multiple levels")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "basic let with multiple levels")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "basic let with multiple levels"
             35
             (let ((x 7) (z (+ x y)))
               (* z x)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "basic let*")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "basic let*")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "basic let*"
             70
             (let ((z (+ x y)))
               (* z x)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "interior define")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "interior define")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "interior define"
             -2
             (let ((x 2) (f 123))
               (let ((_____f1 (λ () (- x))))
                 (begin (set! f _____f1) (f)))))))))

(define let*-def
  1)

#false

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Redefine top level with interior define, stays the same")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Redefine top level with interior define, stays the same")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Redefine top level with interior define, stays the same"
             1
             1)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "simple quasiquote and unquote")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "simple quasiquote and unquote")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "simple quasiquote and unquote"
             (quote
               (list 3 4))
             (cons
                (quote
                  list)
                (cons 3 (cons (quote 4) (quote ())))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "quasiquote and unquote with more")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "quasiquote and unquote with more")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "quasiquote and unquote with more"
             (quote
               (list a (quote a)))
             (let ((name (quote a)))
               (cons
                  (quote
                    list)
                  (cons
                     name
                     (cons
                        (quote
                          (quote
                            (unquote name)))
                        (quote
                          ()))))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "unquote splicing")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "unquote splicing")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "unquote splicing"
             (quote
               (a 3 4 5 6 b))
             (cons
                (quote
                  a)
                (cons
                   3
                   (append
                      (map abs (quote (4 -5 6)))
                      (cons (quote b) (quote ()))))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "unquote splicing with unquote")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "unquote splicing with unquote")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "unquote splicing with unquote"
             (quote
               (10 5 4 16 9 8))
             (cons
                (quote
                  10)
                (cons
                   (quote
                     5)
                   (cons
                      (expt 2 2)
                      (append
                         (map
                            (λ (n)
                              (expt n 2))
                            (quote
                              (4 3)))
                         (cons
                            (quote
                              8)
                            (quote
                              ())))))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "named quasiquote")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "named quasiquote")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "named quasiquote"
             (quote
               (list 3 4))
             (cons
                (quote
                  list)
                (cons 3 (cons (quote 4) (quote ())))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Symbols are interned")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Symbols are interned")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Symbols are interned"
             #true
             (eq? (quote a) (quote a)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "lists don't get interned")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "lists don't get interned")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "lists don't get interned"
             #false
             (eq? (quote (a)) (quote (a))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Empty lists are interned")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Empty lists are interned")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Empty lists are interned"
             #true
             (eq? (quote ()) (quote ())))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "functions are equal")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "functions are equal")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "functions are equal"
             #true
             (eq? car car))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Local vars that are constant point to the same object")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Local vars that are constant point to the same object")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Local vars that are constant point to the same object"
             #true
             (eq? x x))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Function objects are eq? via pointer equality")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Function objects are eq? via pointer equality")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Function objects are eq? via pointer equality"
             #true
             (let ((p (λ (x) x)))
               (eq? p p)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Value equality for interned symbols")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Value equality for interned symbols")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Value equality for interned symbols"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Value equality for interned lists")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Value equality for interned lists")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Value equality for interned lists"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Value equality for nested interned lists")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Value equality for nested interned lists")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Value equality for nested interned lists"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "String equality")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "String equality")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "String equality"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "String inequality")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "String inequality")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "String inequality"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "String inequality, first char")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "String inequality, first char")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "String inequality, first char"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Integer equality")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Integer equality")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Integer equality"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "max over ints")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "max over ints")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "max over ints"
             4
             (max 3 4))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Addition binop")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Addition binop")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Addition binop"
             7
             7)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Addition unary op")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Addition unary op")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Addition unary op"
             3
             3)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Addition no args")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Addition no args")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Addition no args"
             0
             0)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Multiplication one arg, int")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Multiplication one arg, int")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Multiplication one arg, int"
             4
             4)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Multiplication no args, int ")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Multiplication no args, int ")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Multiplication no args, int "
             1
             1)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Subtraction binop")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Subtraction binop")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Subtraction binop"
             -1
             -1)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Subtract three args")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Subtract three args")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Subtract three args"
             -6
             -6)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Subtraction unary op")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Subtraction unary op")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Subtraction unary op"
             -3
             -3)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Subtraction, floating point and int")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Subtraction, floating point and int")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Subtraction, floating point and int"
             -1.0
             -1.0)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "abs int")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "abs int")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "abs int"
             7
             (abs -7))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "integers are truthy")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "integers are truthy")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "integers are truthy"
             #false
             (not 3))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "lists are truthy")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "lists are truthy")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "lists are truthy"
             #false
             (not (quote (3))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty lists are true")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty lists are true")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty lists are true"
             #false
             (not (quote ())))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty lists are true, constructor")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty lists are true, constructor")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty lists are true, constructor"
             #false
             (not (quote ())))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "ints are not bools")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "ints are not bools")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "ints are not bools"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty list is not a boolean")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty list is not a boolean")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty list is not a boolean"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "lists are considered pairs")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "lists are considered pairs")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "lists are considered pairs"
             #true
             (pair? (quote (a b c))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "cons onto empty list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "cons onto empty list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "cons onto empty list"
             (quote
               (a))
             (cons (quote a) (quote ())))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "cons list onto list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "cons list onto list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "cons list onto list"
             (quote
               ((a) b c d))
             (cons (quote (a)) (quote (b c d))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "cons string onto list of symbols")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "cons string onto list of symbols")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "cons string onto list of symbols"
             (quote
               ("a" b c))
             (cons "a" (quote (b c))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "take the car of a list of symbols")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "take the car of a list of symbols")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "take the car of a list of symbols"
             (quote
               a)
             (quote
               a))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "take the car, where the car is a list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "take the car, where the car is a list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "take the car, where the car is a list"
             (quote
               (a))
             (quote
               (a)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "take the cdr of a list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "take the cdr of a list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "take the cdr of a list"
             (quote
               (b c d))
             (cdr (quote ((a) b c d))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Check list predicate")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Check list predicate")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Check list predicate"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Empty list is a list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Empty list is a list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Empty list is a list"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "List constructor")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "List constructor")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "List constructor"
             (quote
               (a 7 c))
             (quote
               (a 7 c)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty list constructor")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty list constructor")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty list constructor"
             (quote
               ())
             (quote
               ()))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "length of a flat list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "length of a flat list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "length of a flat list"
             3
             3)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "length of a non flat list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "length of a non flat list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "length of a non flat list"
             3
             3)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty list has a length of 0")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty list has a length of 0")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty list has a length of 0"
             0
             0)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "append two lists")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "append two lists")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "append two lists"
             (quote
               (x y))
             (append (quote (x)) (quote (y))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "append big list to small list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "append big list to small list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "append big list to small list"
             (quote
               (a b c d))
             (append (quote (a)) (quote (b c d))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "append nested lists")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "append nested lists")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "append nested lists"
             (quote
               (a (b) (c)))
             (append (quote (a (b))) (quote ((c)))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "append to empty list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "append to empty list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "append to empty list"
             (quote
               a)
             (append (quote ()) (quote a)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "reverse list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "reverse list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "reverse list"
             (quote
               (c b a))
             (quote
               (c b a)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "reverse nested list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "reverse nested list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "reverse nested list"
             (quote
               ((e (f)) d (b c) a))
             (quote
               ((e (f)) d (b c) a)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "simple list-ref")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "simple list-ref")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "simple list-ref"
             (quote
               c)
             (list-ref (quote (a b c d)) 2))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "simple member")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "simple member")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "simple member"
             (quote
               ((a) c))
             (member (quote (a)) (quote (b (a) c))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol predicate")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol predicate")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol predicate"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol predicate from constant list")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol predicate from constant list")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol predicate from constant list"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol predicate fails on string")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol predicate fails on string")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol predicate fails on string"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "nil symbol is symbol")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "nil symbol is symbol")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "nil symbol is symbol"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty list is not a symbol")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty list is not a symbol")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty list is not a symbol"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol->string basic case")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol->string basic case")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol->string basic case"
             "flying-fish"
             (symbol->string (quote flying-fish)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol-string works")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol-string works")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol-string works"
             "Martin"
             (symbol->string (quote Martin)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string losslessly moves into symbol and back")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string losslessly moves into symbol and back")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string losslessly moves into symbol and back"
             "Malvina"
             (symbol->string
                (string->symbol "Malvina")))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string predicate correctly identifies a string")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string predicate correctly identifies a string")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string predicate correctly identifies a string"
             #true
             #true)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string predicate fails on a symbol")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string predicate fails on a symbol")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string predicate fails on a symbol"
             #false
             #false)))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "empty string has a length of 0")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "empty string has a length of 0")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "empty string has a length of 0"
             0
             (string-length ""))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string length correctly reported for standard string")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string length correctly reported for standard string")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string length correctly reported for standard string"
             3
             (string-length "abc"))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string-append with empty string")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string-append with empty string")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string-append with empty string"
             "abc"
             "abc")))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string-append with empty string on the lhs")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string-append with empty string on the lhs")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string-append with empty string on the lhs"
             "abc"
             "abc")))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "string-append with two non empty strings")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "string-append with two non empty strings")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "string-append with two non empty strings"
             "abc"
             "abc")))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "function correctly identified as a procedure")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "function correctly identified as a procedure")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "function correctly identified as a procedure"
             #true
             (procedure? car))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "symbol correctly identified as NOT as a procedure")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "symbol correctly identified as NOT as a procedure")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "symbol correctly identified as NOT as a procedure"
             #false
             (procedure? (quote car)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "user defined function correctly identified as a procedure")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "user defined function correctly identified as a procedure")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "user defined function correctly identified as a procedure"
             #true
             (procedure? (λ (x) (* x x))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "quoted expression correctly identified as NOT as procedure")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "quoted expression correctly identified as NOT as procedure")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "quoted expression correctly identified as NOT as procedure"
             #false
             (procedure? (quote (lambda (x) (* x x)))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "basic call/cc with native predicate")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "basic call/cc with native predicate")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "basic call/cc with native predicate"
             #true
             (call-with-current-continuation
                procedure?))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "basic call/cc with user defined function")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "basic call/cc with user defined function")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "basic call/cc with user defined function"
             7
             (call-with-current-continuation (λ (k) 7)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "more complex call/cc with user defined function")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "more complex call/cc with user defined function")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "more complex call/cc with user defined function"
             3
             (call-with-current-continuation
                (λ (k)
                  (+ 2 5 (k 3)))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "apply with native function")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "apply with native function")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "apply with native function"
             7
             (apply + (quote (3 4))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "map with user defined function")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "map with user defined function")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "map with user defined function"
             (quote
               (b e h))
             (map cadr (quote ((a b) (d e) (g h)))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "map with numeric op")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "map with numeric op")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "map with numeric op"
             (quote
               (1 4 27 256 3125))
             (map
                (λ (n)
                  (expt n n))
                (quote
                  (1 2 3 4 5))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "map with multiple list arguments")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "map with multiple list arguments")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "map with multiple list arguments"
             (quote
               (5 7 9))
             (map + (quote (1 2 3)) (quote (4 5 6))))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "using else as a variable")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "using else as a variable")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "using else as a variable"
             (quote
               ok)
             (quote
               ok))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Using an arrow as a variable")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Using an arrow as a variable")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Using an arrow as a variable"
             (quote
               ok)
             (begin 1 (quote ok)))))))

(*reset
   (λ ()
     (call-with-exception-handler
        (λ (##err)
          (begin
                (let ((##err4 ##err))
                  (begin
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmmark-failed
                           "Multiple mutations inside local context")
                        (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmprint-failure
                           "Multiple mutations inside local context")
                        (displayln ##err4)))
                (*shift (λ (k) (k void)))))
        (λ ()
          (mangler/home/matt/Documents/steel/cogs/tests/unit-test.scmtest
             "Multiple mutations inside local context"
             (quote
               (2 3))
             (let ((x 1))
               (let ((y x))
                 (begin
                       (set! x 2)
                       (set! y 3)
                       (list x y)))))))))
