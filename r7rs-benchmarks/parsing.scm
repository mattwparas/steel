;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2006 William D Clinger.
;;
;; Permission to copy this software, in whole or in part, to use this
;; software for any lawful purpose, and to redistribute this software
;; is granted subject to the restriction that all copies made of this
;; software must include this copyright notice in full.
;;
;; I also request that you send me a copy of any improvements that you
;; make to this software so that they may be incorporated within it to
;; the benefit of the Scheme community.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parsing benchmark.
;;
;; Reads nboyer.sch into a string before timing begins.
;;
;; The timed portion of the benchmark parses the string
;; representation of nboyer.sch 1000 times.
;;
;; The output of that parse is checked by comparing it
;; the the value returned by the read procedure.
;;
;; Usage:
;;     (parsing-benchmark n input)
;;
;; n defaults to 1000, and input defaults to "nboyer.sch".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "common.scm")

(define (parsing-benchmark . rest)
  (let* ([n (if (null? rest)
                1000
                (car rest))]
         [input (if (or (null? rest) (null? (cdr rest)))
                    "nboyer.sch"
                    (cadr rest))]
         [input-string (read-file-as-string input)])
    (let loop ([i 0]
               [result #f])
      (if (= i n)
          result
          (loop (+ i 1) (parse-string input-string))))))

(define (read-file-as-string name)
  (call-with-input-file name
                        (lambda (in)
                          (do ((x (read-char in) (read-char in)) (chars '() (cons x chars)))
                              ((eof-object? x) (list->string (reverse chars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The parser used for benchmarking.
;;
;; Given a string containing Scheme code, parses the entire
;; string and returns the last <datum> read from the string.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-string input-string)

  ;; Constants and local variables.

  (let* (; Constants.

         ;; Any character that doesn't appear within nboyer.sch
         ;; (or the input file, if different) can be used to
         ;; represent end-of-file.

         [eof #\~]

         ;; length of longest token allowed
         ;; (this allows static allocation in C)

         [max_token_size 1024]

         ;; Encodings of error messages.

         [errLongToken 1] ;; extremely long token
         [errincompletetoken 2] ;; any lexical error, really
         [errLexGenBug 3] ;; can't happen

         ;; State for one-token buffering in lexical analyzer.

         [kindOfNextToken 'z1] ;; valid iff nextTokenIsReady
         [nextTokenIsReady #f]

         [tokenValue ""] ;; string associated with current token

         [totalErrors 0] ;; errors so far
         [lineNumber 1] ;; rudimentary source code location
         [lineNumberOfLastError 0] ;; ditto

         ;; A string buffer for the characters of the current token.

         [string_accumulator (make-string max_token_size)]

         ;; Number of characters in string_accumulator.

         [string_accumulator_length 0]

         ;; A single character of buffering.
         ;; nextCharacter is valid iff nextCharacterIsReady

         [nextCharacter #\space]
         [nextCharacterIsReady #f]

         ;; Index of next character to be read from input-string.

         [input-index 0]

         [input-length (string-length input-string)])

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; LexGen generated the code for the state machine.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (scanner0)
      (let loop ([c (scanchar)])
        (when (char-whitespace? c)
          (consumechar)
          (set! string_accumulator_length 0)
          (loop (scanchar))))
      (let ([c (scanchar)])
        (if (char=? c eof)
            (accept 'eof)
            (state0 c))))

    (define (state0 c)
      (case c
        [(#\`)
         (consumechar)
         (accept 'backquote)]
        [(#\')
         (consumechar)
         (accept 'quote)]
        [(#\))
         (consumechar)
         (accept 'rparen)]
        [(#\()
         (consumechar)
         (accept 'lparen)]
        [(#\;)
         (consumechar)
         (state29 (scanchar))]
        [(#\+ #\-)
         (consumechar)
         (state28 (scanchar))]
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state27 (scanchar))]
        [(#\.)
         (consumechar)
         (state16 (scanchar))]
        [(#\a #\b
              #\c
              #\d
              #\e
              #\f
              #\g
              #\h
              #\i
              #\j
              #\k
              #\l
              #\m
              #\n
              #\o
              #\p
              #\q
              #\r
              #\s
              #\t
              #\u
              #\v
              #\w
              #\x
              #\y
              #\z
              #\A
              #\B
              #\C
              #\D
              #\E
              #\F
              #\G
              #\H
              #\I
              #\J
              #\K
              #\L
              #\M
              #\N
              #\O
              #\P
              #\Q
              #\R
              #\S
              #\T
              #\U
              #\V
              #\W
              #\X
              #\Y
              #\Z
              #\!
              #\$
              #\%
              #\&
              #\*
              #\/
              #\:
              #\<
              #\=
              #\>
              #\?
              #\^
              #\_
              #\~)
         (consumechar)
         (state14 (scanchar))]
        [(#\#)
         (consumechar)
         (state13 (scanchar))]
        [(#\")
         (consumechar)
         (state2 (scanchar))]
        [(#\,)
         (consumechar)
         (state1 (scanchar))]
        [else
         (if (char-whitespace? c)
             (begin
               (consumechar)
               (state30 (scanchar)))
             (scannererror errincompletetoken))]))
    (define (state1 c)
      (case c
        [(#\@)
         (consumechar)
         (accept 'splicing)]
        [else (accept 'comma)]))
    (define (state2 c)
      (case c
        [(#\")
         (consumechar)
         (accept 'string)]
        [else
         (if (isnotdoublequote? c)
             (begin
               (consumechar)
               (state2 (scanchar)))
             (scannererror errincompletetoken))]))
    (define (state3 c)
      (case c
        [(#\n)
         (consumechar)
         (state8 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state4 c)
      (case c
        [(#\i)
         (consumechar)
         (state3 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state5 c)
      (case c
        [(#\l)
         (consumechar)
         (state4 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state6 c)
      (case c
        [(#\w)
         (consumechar)
         (state5 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state7 c)
      (case c
        [(#\e)
         (consumechar)
         (state6 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state8 c)
      (case c
        [(#\e)
         (consumechar)
         (accept 'character)]
        [else (scannererror errincompletetoken)]))
    (define (state9 c)
      (case c
        [(#\c)
         (consumechar)
         (state8 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state10 c)
      (case c
        [(#\a)
         (consumechar)
         (state9 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state11 c)
      (case c
        [(#\p)
         (consumechar)
         (state10 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state12 c)
      (case c
        [(#\s)
         (consumechar)
         (state11 (scanchar))]
        [(#\n)
         (consumechar)
         (state7 (scanchar))]
        [else
         (if (char? c)
             (begin
               (consumechar)
               (accept 'character))
             (scannererror errincompletetoken))]))
    (define (state13 c)
      (case c
        [(#\()
         (consumechar)
         (accept 'vecstart)]
        [(#\t #\f)
         (consumechar)
         (accept 'boolean)]
        [(#\\)
         (consumechar)
         (state12 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state14 c)
      (case c
        [(#\a #\b
              #\c
              #\d
              #\e
              #\f
              #\g
              #\h
              #\i
              #\j
              #\k
              #\l
              #\m
              #\n
              #\o
              #\p
              #\q
              #\r
              #\s
              #\t
              #\u
              #\v
              #\w
              #\x
              #\y
              #\z
              #\A
              #\B
              #\C
              #\D
              #\E
              #\F
              #\G
              #\H
              #\I
              #\J
              #\K
              #\L
              #\M
              #\N
              #\O
              #\P
              #\Q
              #\R
              #\S
              #\T
              #\U
              #\V
              #\W
              #\X
              #\Y
              #\Z
              #\!
              #\$
              #\%
              #\&
              #\*
              #\/
              #\:
              #\<
              #\=
              #\>
              #\?
              #\^
              #\_
              #\~
              #\0
              #\1
              #\2
              #\3
              #\4
              #\5
              #\6
              #\7
              #\8
              #\9
              #\+
              #\-
              #\.
              #\@)
         (consumechar)
         (state14 (scanchar))]
        [else (accept 'id)]))
    (define (state15 c)
      (case c
        [(#\.)
         (consumechar)
         (accept 'id)]
        [else (scannererror errincompletetoken)]))
    (define (state16 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state18 (scanchar))]
        [(#\.)
         (consumechar)
         (state15 (scanchar))]
        [else (accept 'period)]))
    (define (state17 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state18 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state18 c)
      (case c
        [(#\e #\s #\f #\d #\l)
         (consumechar)
         (state22 (scanchar))]
        [(#\#)
         (consumechar)
         (state19 (scanchar))]
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state18 (scanchar))]
        [else (accept 'number)]))
    (define (state19 c)
      (case c
        [(#\e #\s #\f #\d #\l)
         (consumechar)
         (state22 (scanchar))]
        [(#\#)
         (consumechar)
         (state19 (scanchar))]
        [else (accept 'number)]))
    (define (state20 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state20 (scanchar))]
        [else (accept 'number)]))
    (define (state21 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state20 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state22 c)
      (case c
        [(#\+ #\-)
         (consumechar)
         (state21 (scanchar))]
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state20 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state23 c)
      (case c
        [(#\#)
         (consumechar)
         (state23 (scanchar))]
        [else (accept 'number)]))
    (define (state24 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state24 (scanchar))]
        [(#\#)
         (consumechar)
         (state23 (scanchar))]
        [else (accept 'number)]))
    (define (state25 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state24 (scanchar))]
        [else (scannererror errincompletetoken)]))
    (define (state26 c)
      (case c
        [(#\#)
         (consumechar)
         (state26 (scanchar))]
        [(#\/)
         (consumechar)
         (state25 (scanchar))]
        [(#\e #\s #\f #\d #\l)
         (consumechar)
         (state22 (scanchar))]
        [(#\.)
         (consumechar)
         (state19 (scanchar))]
        [else (accept 'number)]))
    (define (state27 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state27 (scanchar))]
        [(#\#)
         (consumechar)
         (state26 (scanchar))]
        [(#\/)
         (consumechar)
         (state25 (scanchar))]
        [(#\e #\s #\f #\d #\l)
         (consumechar)
         (state22 (scanchar))]
        [(#\.)
         (consumechar)
         (state18 (scanchar))]
        [else (accept 'number)]))
    (define (state28 c)
      (case c
        [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (consumechar)
         (state27 (scanchar))]
        [(#\.)
         (consumechar)
         (state17 (scanchar))]
        [else (accept 'id)]))
    (define (state29 c)
      (case c
        [(#\newline)
         (consumechar)
         (begin
           (set! string_accumulator_length 0)
           (state0 (scanchar)))]
        [else
         (if (isnotnewline? c)
             (begin
               (consumechar)
               (state29 (scanchar)))
             (scannererror errincompletetoken))]))
    (define (state30 c)
      (case c
        [else
         (if (char-whitespace? c)
             (begin
               (consumechar)
               (state30 (scanchar)))
             (begin
               (set! string_accumulator_length 0)
               (state0 (scanchar))))]))
    (define (state31 c)
      (case c
        [else
         (begin
           (set! string_accumulator_length 0)
           (state0 (scanchar)))]))
    (define (state32 c)
      (case c
        [else (accept 'id)]))
    (define (state33 c)
      (case c
        [else (accept 'boolean)]))
    (define (state34 c)
      (case c
        [else (accept 'character)]))
    (define (state35 c)
      (case c
        [else (accept 'vecstart)]))
    (define (state36 c)
      (case c
        [else (accept 'string)]))
    (define (state37 c)
      (case c
        [else (accept 'lparen)]))
    (define (state38 c)
      (case c
        [else (accept 'rparen)]))
    (define (state39 c)
      (case c
        [else (accept 'quote)]))
    (define (state40 c)
      (case c
        [else (accept 'backquote)]))
    (define (state41 c)
      (case c
        [else (accept 'splicing)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; End of state machine generated by LexGen.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; ParseGen generated the code for the strong LL(1) parser.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (parse-datum)
      (case (next-token)
        [(splicing comma backquote quote lparen vecstart)
         (let ([ast1 (parse-compound-datum)]) (identity ast1))]
        [(boolean number character string id) (let ([ast1 (parse-simple-datum)]) (identity ast1))]
        [else
         (parse-error
          '<datum>
          '(backquote boolean character comma id lparen number quote splicing string vecstart))]))

    (define (parse-simple-datum)
      (case (next-token)
        [(id) (let ([ast1 (parse-symbol)]) (identity ast1))]
        [(string)
         (begin
           (consume-token!)
           (makeString))]
        [(character)
         (begin
           (consume-token!)
           (makeChar))]
        [(number)
         (begin
           (consume-token!)
           (makeNum))]
        [(boolean)
         (begin
           (consume-token!)
           (makeBool))]
        [else (parse-error '<simple-datum> '(boolean character id number string))]))

    (define (parse-symbol)
      (case (next-token)
        [(id)
         (begin
           (consume-token!)
           (makeSym))]
        [else (parse-error '<symbol> '(id))]))

    (define (parse-compound-datum)
      (case (next-token)
        [(vecstart) (let ([ast1 (parse-vector)]) (identity ast1))]
        [(lparen quote backquote comma splicing) (let ([ast1 (parse-list)]) (identity ast1))]
        [else (parse-error '<compound-datum> '(backquote comma lparen quote splicing vecstart))]))

    (define (parse-list)
      (case (next-token)
        [(splicing comma backquote quote) (let ([ast1 (parse-abbreviation)]) (identity ast1))]
        [(lparen)
         (begin
           (consume-token!)
           (let ([ast1 (parse-list2)]) (identity ast1)))]
        [else (parse-error '<list> '(backquote comma lparen quote splicing))]))

    (define (parse-list2)
      (case (next-token)
        [(id string character number boolean vecstart lparen quote backquote comma splicing)
         (let ([ast1 (parse-datum)]) (let ([ast2 (parse-list3)]) (cons ast1 ast2)))]
        [(rparen)
         (begin
           (consume-token!)
           (emptyList))]
        [else
         (parse-error '<list2>
                      '(backquote boolean
                                  character
                                  comma
                                  id
                                  lparen
                                  number
                                  quote
                                  rparen
                                  splicing
                                  string
                                  vecstart))]))

    (define (parse-list3)
      (case (next-token)
        [(rparen period
                 splicing
                 comma
                 backquote
                 quote
                 lparen
                 vecstart
                 boolean
                 number
                 character
                 string
                 id)
         (let ([ast1 (parse-data)]) (let ([ast2 (parse-list4)]) (pseudoAppend ast1 ast2)))]
        [else
         (parse-error '<list3>
                      '(backquote boolean
                                  character
                                  comma
                                  id
                                  lparen
                                  number
                                  period
                                  quote
                                  rparen
                                  splicing
                                  string
                                  vecstart))]))

    (define (parse-list4)
      (case (next-token)
        [(period)
         (begin
           (consume-token!)
           (let ([ast1 (parse-datum)])
             (if (eq? (next-token) 'rparen)
                 (begin
                   (consume-token!)
                   (identity ast1))
                 (parse-error '<list4> '(rparen)))))]
        [(rparen)
         (begin
           (consume-token!)
           (emptyList))]
        [else (parse-error '<list4> '(period rparen))]))

    (define (parse-abbreviation)
      (case (next-token)
        [(quote backquote comma splicing)
         (let ([ast1 (parse-abbrev-prefix)]) (let ([ast2 (parse-datum)]) (list ast1 ast2)))]
        [else (parse-error '<abbreviation> '(backquote comma quote splicing))]))

    (define (parse-abbrev-prefix)
      (case (next-token)
        [(splicing)
         (begin
           (consume-token!)
           (symSplicing))]
        [(comma)
         (begin
           (consume-token!)
           (symUnquote))]
        [(backquote)
         (begin
           (consume-token!)
           (symBackquote))]
        [(quote)
         (begin
           (consume-token!)
           (symQuote))]
        [else (parse-error '<abbrev-prefix> '(backquote comma quote splicing))]))

    (define (parse-vector)
      (case (next-token)
        [(vecstart)
         (begin
           (consume-token!)
           (let ([ast1 (parse-data)])
             (if (eq? (next-token) 'rparen)
                 (begin
                   (consume-token!)
                   (list2vector ast1))
                 (parse-error '<vector> '(rparen)))))]
        [else (parse-error '<vector> '(vecstart))]))

    (define (parse-data)
      (case (next-token)
        [(id string character number boolean vecstart lparen quote backquote comma splicing)
         (let ([ast1 (parse-datum)]) (let ([ast2 (parse-data)]) (cons ast1 ast2)))]
        [(rparen period) (emptyList)]
        [else
         (parse-error '<data>
                      '(backquote boolean
                                  character
                                  comma
                                  id
                                  lparen
                                  number
                                  period
                                  quote
                                  rparen
                                  splicing
                                  string
                                  vecstart))]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; End of LL(1) parser generated by ParseGen.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Help predicates used by the lexical analyzer's state machine.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (isnotdoublequote? c)
      (not (char=? c #\")))
    (define (isnotnewline? c)
      (not (char=? c #\newline)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Lexical analyzer.
    ;;
    ;; This code is adapted from the quirk23 lexical analyzer written
    ;; by Will Clinger for a compiler course.
    ;;
    ;; The scanner and parser were generated automatically and then
    ;; printed using an R5RS Scheme pretty-printer, so they do not
    ;; preserve case.  In preparation for the case-sensitivity of
    ;; R6RS Scheme, several identifiers and constants have been
    ;; lower-cased in the hand-written code to match the generated
    ;; code.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; next-token and consume-token! are called by the parser.

    ;; Returns the current token.

    (define (next-token)
      (if nextTokenIsReady
          kindOfNextToken
          (begin
            (set! string_accumulator_length 0)
            (scanner0))))

    ;; Consumes the current token.

    (define (consume-token!)
      (set! nextTokenIsReady #f))

    ;; Called by the lexical analyzer's state machine,
    ;; hence the unfortunate lower case.

    (define (scannererror msg)
      (define msgtxt
        (cond
          [(= msg errLongToken) "Amazingly long token"]
          [(= msg errincompletetoken) "in line "]
          [(= msg errLexGenBug) "Bug in lexical analyzer (generated)"]
          [else "Bug in lexical analyzer"]))
      (error #f (string-append "Lexical Error: " msgtxt) lineNumber)
      (set! nextTokenIsReady #f)
      (set! nextCharacterIsReady #f)
      (next-token))

    ;; Accepts a token of the given kind, returning that kind.
    ;;
    ;; For some kinds of tokens, a value for the token must also be
    ;; recorded in tokenValue.

    (define (accept t)
      (when (memq t '(boolean character id number string))
        (set! tokenValue (substring string_accumulator 0 string_accumulator_length)))
      (set! kindOfNextToken t)
      (set! nextTokenIsReady #t)
      t)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Character i/o, so to speak.
    ;; Uses the input-string as input.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Returns the current character from the input.

    (define (scanchar)
      (if nextCharacterIsReady
          nextCharacter
          (begin
            (if (< input-index input-length)
                (begin
                  (set! nextCharacter (string-ref input-string input-index))
                  (set! input-index (+ input-index 1)))
                (set! nextCharacter eof))
            (set! nextCharacterIsReady #t)
            ;; For debugging, change #f to #t below.
            (when #f
              (write-char nextCharacter))
            (scanchar))))

    ;; Consumes the current character, and returns the next.

    (define (consumechar)
      (unless nextCharacterIsReady
        (scanchar))
      (if (< string_accumulator_length max_token_size)
          (begin
            (set! nextCharacterIsReady #f)
            (when (char=? nextCharacter #\newline)
              (set! lineNumber (+ lineNumber 1)))
            (string-set! string_accumulator string_accumulator_length nextCharacter)
            (set! string_accumulator_length (+ string_accumulator_length 1)))
          (scannererror errLongToken)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Action procedures called by the parser.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (emptyList)
      '())

    (define (identity x)
      x)

    (define (list2vector vals)
      (list->vector vals))

    (define (makeBool)
      (string=? tokenValue "#t"))

    (define (makeChar)
      (string-ref tokenValue 0))

    (define (makeNum)
      (string->number tokenValue))

    (define (makeString)
      ;; Must strip off outer double quotes.
      ;; Ought to process escape characters also, but we won't.
      (substring tokenValue 1 (- (string-length tokenValue) 1)))

    (define (makeSym)
      (string->symbol tokenValue))

    ;; Like append, but allows the last argument to be a non-list.

    (define (pseudoAppend vals terminus)
      (if (null? vals)
          terminus
          (cons (car vals) (pseudoAppend (cdr vals) terminus))))

    (define (symBackquote)
      'quasiquote)
    (define (symQuote)
      'quote)
    (define (symSplicing)
      'unquote-splicing)
    (define (symUnquote)
      'unquote)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Error procedure called by the parser.
    ;; As a hack, this error procedure recovers from end-of-file.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (parse-error nonterminal expected-terminals)
      (if (eq? 'eof (next-token))
          'eof
          (begin
            (display "Syntax error in line ")
            (display lineNumber)
            (display " while parsing a ")
            (write nonterminal)
            (newline)
            (display "  Encountered a ")
            (display (next-token))
            (display " while expecting something in")
            (newline)
            (display "  ")
            (write expected-terminals)
            (newline)
            (error #f "Syntax error"))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Parses repeatedly, returning the last <datum> parsed.
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (do ((x (parse-datum) (parse-datum)) (y 'eof x)) ((eq? x 'eof) y))))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s2 (number->string count)]
         [s1 input1]
         [name "parsing"])
    (run-r7rs-benchmark (string-append name ":" s2)
                        1
                        (lambda () (parsing-benchmark (hide count count) (hide count input1)))
                        (lambda (result) (equal? result output)))))
