(provide ArgumentParsingResult-positional-args
         make-command-line-arg-parser
         get-option)

;; Bring in the options library
; (require "steel/option")

;; Options - these become things like:
;; --help
;; --foo bar
;; etc.

; (define test-args '("foo" "bar" "--optional-arg" "bananas"))

;; Setup the argument parser spec
(struct ArgumentParserSpec (subcommands positional-args required-args optional-args))

(struct ArgumentParsingResult (positional-args required-args optional-args) #:transparent)

;; Build up the args by crunching through, eagerly assigning
;; to the respective fields. If there is no match for a subcommand, then we
;; simply continue.
(define (parse spec arg-list positional-args required-args optional-args)

  (cond
    [(empty? arg-list)
     ;; Check that the positionl args have been fulfilled
     (when (not (= (length (ArgumentParserSpec-positional-args spec)) (length positional-args)))
       (error "Missing positional arguments: "
              (drop (ArgumentParserSpec-positional-args spec) (length positional-args))))

     (ArgumentParsingResult positional-args required-args optional-args)]

    [else
     (define next (car arg-list))

     (cond
       [(starts-with? next "--")
        (define arg-name (trim-start-matches next "--"))

        (cond
          [(hash-contains? (ArgumentParserSpec-required-args spec) arg-name)
           ;; Rework this so the list goes later?
           (parse spec
                  (cddr arg-list)
                  positional-args
                  (hash-insert required-args arg-name (cadr arg-list))
                  optional-args)]

          ;; Optional arguments are those where the presence of the flag
          ;; just dictates that the flag is enabled, not that we need to
          ;; eagerly parse the next argument.
          [(hash-contains? (ArgumentParserSpec-optional-args spec) arg-name)

           ;; The existence of the flag means its enabled. But, on the off chance
           ;; that the flag value exists, take it.
           (parse spec
                  (cdr arg-list)
                  positional-args
                  required-args
                  (hash-insert optional-args arg-name #t))]

          [else (error "Unrecognized command line argument: " arg-name)])]

       [(starts-with? next "-")

        (define arg-name (trim-start-matches next "-"))

        (error "todo!")]

       [else
        ;; We've already collected all of the arguments we're expecting, so we
        ;; should just bail out of this one
        (when (= (length positional-args) (length (ArgumentParserSpec-positional-args spec)))
          (error "Unexpected positional argument: " (car arg-list)))

        ;; Anything else, just gets lumped in with the standard arguments
        (parse spec
               (cdr arg-list)
               (cons (car arg-list) positional-args)
               required-args
               optional-args)])]))

;; Declarative API?
(define-syntax command-line-args
  (syntax-rules ()
    [(_) (error "TODO")]))

;; Create a command line parser, given a spec
(define (make-command-line-arg-parser
         #:positional [positional-args '()]
         ;; List of pairs, argument with doc - previously a hashset
         #:required [required-args '()]
         ;; List of triples, argument with doc, default value - previously a hash of key -> default
         #:optional [optional-args '()])

  ;; Map from key -> doc
  (define required-docs (transduce required-args (mapping (lambda (p) p)) (into-hashmap)))
  (define optional-docs
    (transduce optional-args (mapping (lambda (p) (cons (caar p) (cadr p)))) (into-hashmap)))

  ;; Keep track of all of the docs
  (define all-docs (hash-union required-docs optional-docs))

  (define required-args (transduce required-args (mapping (lambda (p) (car p))) (into-hashmap)))
  ;; Add the help options
  (define optional-args
    (~> (transduce optional-args (mapping (lambda (p) (car p))) (into-hashmap))
        (hash-insert "help" #f)))

  (define local-spec (ArgumentParserSpec '() positional-args required-args optional-args))

  (case-lambda
    [() (parse local-spec (drop (command-line) 2) '() required-args optional-args)]
    [(command-line-args) (parse local-spec command-line-args '() required-args optional-args)]))

;; Setup some kind of macro system to declare the interface, and in theory
;; it should resolve to the right things.

;; Check the value, otherwise
(define (get-option spec option)
  (define required (ArgumentParsingResult-required-args spec))
  (define optional (ArgumentParsingResult-optional-args spec))
  (if (hash-contains? required option) (hash-ref required option) (hash-ref optional option)))

(define my-options
  (make-command-line-arg-parser #:positional (list '("path" "The input path to read")
                                                   '("output" "The output path to read"))
                                #:required '((("required-arg-1" #f) "Setting up the values"))))
