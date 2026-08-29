(require "common.scm")
(require "mutable-lists.scm")
(require "mutable-strings.scm")
(require "char-compat.scm")

(define file-exists? path-exists?)
(define delete-file delete-file!)

;;; SLATEX -- Scheme to Latex processor.

                                        ;slatex.scm file generated using config.scm
                                        ;This file is compatible for the dialect other
                                        ;(c) Dorai Sitaram, Rice U., 1991, 1994

(define *op-sys* 'unix)

(define slatex.ormap
  (lambda (f l)
    (let loop ((l l)) (if (null? l) #f (or (f (car l)) (loop (cdr l)))))))

(define slatex.ormapcdr
  (lambda (f l)
    (let loop ((l l)) (if (null? l) #f (or (f l) (loop (cdr l)))))))

(define slatex.append!
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else
           (let loop ((l1 l1))
             (if (null? (cdr l1)) (set-cdr! l1 l2) (loop (cdr l1))))
           l1))))

(define slatex.append-map!
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) '() (slatex.append! (f (car l)) (loop (cdr l)))))))

(define slatex.remove-if!
  (lambda (p s)
    (let loop ((s s))
      (cond ((null? s) '())
            ((p (car s)) (loop (cdr s)))
            (else (let ((r (loop (cdr s)))) (set-cdr! s r) s))))))

(define slatex.reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r (let ((d (cdr s))) (set-cdr! s r) (loop d s))))))

(define slatex.list-set!
  (lambda (l i v)
    (let loop ((l l) (i i))
      (cond ((null? l) (slatex.error 'slatex.list-set! 'list-too-small))
            ((= i 0) (set-car! l v))
            (else (loop (cdr l) (- i 1)))))))

(define slatex.list-prefix?
  (lambda (pfx l)
    (cond ((null? pfx) #t)
          ((null? l) #f)
          ((eqv? (car pfx) (car l)) (slatex.list-prefix? (cdr pfx) (cdr l)))
          (else #f))))

(define slatex.string-prefix?
  (lambda (pfx s)
    (let ((pfx-len (string-length pfx)) (s-len (string-length s)))
      (if (> pfx-len s-len)
          #f
          (let loop ((i 0))
            (if (>= i pfx-len)
                #t
                (and (char=? (string-ref pfx i) (string-ref s i))
                     (loop (+ i 1)))))))))

(define slatex.string-suffix?
  (lambda (sfx s)
    (let ((sfx-len (string-length sfx)) (s-len (string-length s)))
      (if (> sfx-len s-len)
          #f
          (let loop ((i (- sfx-len 1)) (j (- s-len 1)))
            (if (< i 0)
                #t
                (and (char=? (string-ref sfx i) (string-ref s j))
                     (loop (- i 1) (- j 1)))))))))

(define slatex.member-string member)

(define slatex.adjoin-string
  (lambda (s l) (if (slatex.member-string s l) l (cons s l))))

(define slatex.remove-string!
  (lambda (s l) (slatex.remove-if! (lambda (l_i) (string=? l_i s)) l)))

(define slatex.adjoin-char (lambda (c l) (if (memv c l) l (cons c l))))

(define slatex.remove-char!
  (lambda (c l) (slatex.remove-if! (lambda (l_i) (char=? l_i c)) l)))

(define slatex.sublist
  (lambda (l i f)
    (let loop ((l (list-tail l i)) (k i) (r '()))
      (cond ((>= k f) (slatex.reverse! r))
            ((null? l) (slatex.error 'slatex.sublist 'list-too-small))
            (else (loop (cdr l) (+ k 1) (cons (car l) r)))))))

(define slatex.position-char
  (lambda (c l)
    (let loop ((l l) (i 0))
      (cond ((null? l) #f)
            ((char=? (car l) c) i)
            (else (loop (cdr l) (+ i 1)))))))

(define slatex.string-position-right
  (lambda (c s)
    (let ((n (string-length s)))
      (let loop ((i (- n 1)))
        (cond ((< i 0) #f)
              ((char=? (string-ref s i) c) i)
              (else (loop (- i 1))))))))

(define slatex.token=?
  (lambda (t1 t2)
    ((if slatex.*slatex-case-sensitive?* string=? string-ci=?) t1 t2)))

(define slatex.assoc-token
  (lambda (x s)
    (slatex.ormap (lambda (s_i) (if (slatex.token=? (car s_i) x) s_i #f)) s)))

(define slatex.member-token
  (lambda (x s)
    (slatex.ormapcdr
     (lambda (s_i..) (if (slatex.token=? (car s_i..) x) s_i.. #f))
     s)))

(define slatex.remove-token!
  (lambda (x s) (slatex.remove-if! (lambda (s_i) (slatex.token=? s_i x)) s)))

(define slatex.file-exists? (lambda (f) (file-exists? f)))

(define slatex.delete-file (lambda (f) (delete-file f)))

(define slatex.force-output (lambda z 'assume-output-forced))

(define slatex.*return* (integer->char 13))

(define slatex.*tab* (integer->char 9))

(define slatex.error
  (lambda (error-type error-values)
    (display "Error: ")
    (display error-type)
    (display ": ")
    (newline)
    ;; (for-each (lambda (x) (write x) (newline)) error-values)
    (write error-values) ;; ecraven changed from the line above, slatex.error seems to always be called with an atom as error-values
    (error #f "")))

(define slatex.keyword-tokens
  (map symbol->string
       '(=> %
            abort
            and
            begin
            begin0
            case
            case-lambda
            cond
            define
            define!
            define-macro!
            define-syntax
            defrec!
            delay
            do
            else
            extend-syntax
            fluid-let
            if
            lambda
            let
            let*
            letrec
            let-syntax
            letrec-syntax
            or
            quasiquote
            quote
            rec
            record-case
            record-evcase
            recur
            set!
            sigma
            struct
            syntax
            syntax-rules
            trace
            trace-lambda
            trace-let
            trace-recur
            unless
            unquote
            unquote-splicing
            untrace
            when
            with)))

(define slatex.variable-tokens '())

(define slatex.constant-tokens '())

(define slatex.special-symbols
  (list (cons "." ".")
        (cons "..." "{\\dots}")
        (cons "-" "$-$")
        (cons "1-" "\\va{1$-$}")
        (cons "-1+" "\\va{$-$1$+$}")))

(define slatex.macro-definers
  '("define-syntax" "syntax-rules" "defmacro" "extend-syntax" "define-macro!"))

(define slatex.case-and-ilk '("case" "record-case"))

(define slatex.tex-analog
  (lambda (c)
    (cond ((memv c '(#\$ #\& #\% #\# #\_)) (string #\\ c))
          ((memv c '(#\{ #\})) (string #\$ #\\ c #\$))
          ((char=? c #\\) "$\\backslash$")
          ((char=? c #\+) "$+$")
          ((char=? c #\=) "$=$")
          ((char=? c #\<) "$\\lt$")
          ((char=? c #\>) "$\\gt$")
          ((char=? c #\^) "\\^{}")
          ((char=? c #\|) "$\\vert$")
          ((char=? c #\~) "\\~{}")
          ((char=? c #\@) "{\\atsign}")
          ((char=? c #\") "{\\tt\\dq}")
          (else (string c)))))

(define slatex.*slatex-case-sensitive?* #t)

(define slatex.*slatex-enabled?* #t)

(define slatex.*slatex-reenabler* "UNDEFINED")

(define slatex.*intext-triggerers* (list "scheme"))

(define slatex.*resultintext-triggerers* (list "schemeresult"))

(define slatex.*display-triggerers* (list "schemedisplay"))

(define slatex.*box-triggerers* (list "schemebox"))

(define slatex.*input-triggerers* (list "schemeinput"))

(define slatex.*region-triggerers* (list "schemeregion"))

(define slatex.*math-triggerers* '())

(define slatex.*slatex-in-protected-region?* #f)

(define slatex.*protected-files* '())

(define slatex.*include-onlys* 'all)

(define slatex.*latex?* #t)

(define slatex.*slatex-separate-includes?* #f)

(define slatex.set-keyword
  (lambda (x)
    (if (slatex.member-token x slatex.keyword-tokens)
        'skip
        (begin
          (set! slatex.constant-tokens
                (slatex.remove-token! x slatex.constant-tokens))
          (set! slatex.variable-tokens
                (slatex.remove-token! x slatex.variable-tokens))
          (set! slatex.keyword-tokens (cons x slatex.keyword-tokens))))))

(define slatex.set-constant
  (lambda (x)
    (if (slatex.member-token x slatex.constant-tokens)
        'skip
        (begin
          (set! slatex.keyword-tokens
                (slatex.remove-token! x slatex.keyword-tokens))
          (set! slatex.variable-tokens
                (slatex.remove-token! x slatex.variable-tokens))
          (set! slatex.constant-tokens (cons x slatex.constant-tokens))))))

(define slatex.set-variable
  (lambda (x)
    (if (slatex.member-token x slatex.variable-tokens)
        'skip
        (begin
          (set! slatex.keyword-tokens
                (slatex.remove-token! x slatex.keyword-tokens))
          (set! slatex.constant-tokens
                (slatex.remove-token! x slatex.constant-tokens))
          (set! slatex.variable-tokens (cons x slatex.variable-tokens))))))

(define slatex.set-special-symbol
  (lambda (x transl)
    (let ((c (slatex.assoc-token x slatex.special-symbols)))
      (if c
          (set-cdr! c transl)
          (set! slatex.special-symbols
                (cons (cons x transl) slatex.special-symbols))))))

(define slatex.unset-special-symbol
  (lambda (x)
    (set! slatex.special-symbols
          (slatex.remove-if!
           (lambda (c) (slatex.token=? (car c) x))
           slatex.special-symbols))))

(define slatex.texify (lambda (s) (list->string (slatex.texify-aux s))))

(define slatex.texify-data
  (lambda (s)
    (let loop ((l (slatex.texify-aux s)) (r '()))
      (if (null? l)
          (list->string (slatex.reverse! r))
          (let ((c (car l)))
            (loop (cdr l)
                  (if (char=? c #\-)
                      (slatex.append! (list #\$ c #\$) r)
                      (cons c r))))))))

(define slatex.texify-aux
  (let* ((arrow (string->list "-$>$")) (arrow-lh (length arrow)))
    (lambda (s)
      (let* ((sl (string->list s))
             (texified-sl
              (slatex.append-map!
               (lambda (c) (string->list (slatex.tex-analog c)))
               sl)))
        (slatex.ormapcdr
         (lambda (d)
           (if (slatex.list-prefix? arrow d)
               (let ((to (string->list "$\\to$")))
                 (set-car! d (car to))
                 (set-cdr! d (append (cdr to) (list-tail d arrow-lh)))))
           #f)
         texified-sl)
        texified-sl))))

(define slatex.display-begin-sequence
  (lambda (out)
    (if (or slatex.*intext?* (not slatex.*latex?*))
        (begin
          (display "\\" out)
          (display slatex.*code-env-spec* out)
          (newline out))
        (begin
          (display "\\begin{" out)
          (display slatex.*code-env-spec* out)
          (display "}" out)
          (newline out)))))

(define slatex.display-end-sequence
  (lambda (out)
    (if (or slatex.*intext?* (not slatex.*latex?*))
        (begin
          (display "\\end" out)
          (display slatex.*code-env-spec* out)
          (newline out))
        (begin
          (display "\\end{" out)
          (display slatex.*code-env-spec* out)
          (display "}" out)
          (newline out)))))

(define slatex.display-tex-char
  (lambda (c p) (display (if (char? c) (slatex.tex-analog c) c) p)))

(define slatex.display-token
  (lambda (s typ p)
    (cond ((eq? typ 'syntax)
           (display "\\sy{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'variable)
           (display "\\va{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'constant)
           (display "\\cn{" p)
           (display (slatex.texify s) p)
           (display "}" p))
          ((eq? typ 'data)
           (display "\\dt{" p)
           (display (slatex.texify-data s) p)
           (display "}" p))
          (else (slatex.error 'slatex.display-token typ)))))

(define slatex.*max-line-length* 200)

(begin
  (define slatex.&inner-space (integer->char 7))
  (define slatex.&quote-space (integer->char 6))
  (define slatex.&bracket-space (integer->char 5))
  (define slatex.&paren-space (integer->char 4))
  (define slatex.&init-plain-space (integer->char 3))
  (define slatex.&init-space (integer->char 2))
  (define slatex.&plain-space (integer->char 1))
  (define slatex.&void-space (integer->char 0)))

(begin
  (define slatex.&plain-crg-ret (integer->char 4))
  (define slatex.&tabbed-crg-ret (integer->char 3))
  (define slatex.&move-tab (integer->char 2))
  (define slatex.&set-tab (integer->char 1))
  (define slatex.&void-tab (integer->char 0)))

(begin
  (define slatex.&end-math (integer->char 8))
  (define slatex.&mid-math (integer->char 7))
  (define slatex.&begin-math (integer->char 6))
  (define slatex.&end-string (integer->char 5))
  (define slatex.&mid-string (integer->char 4))
  (define slatex.&begin-string (integer->char 3))
  (define slatex.&mid-comment (integer->char 2))
  (define slatex.&begin-comment (integer->char 1))
  (define slatex.&void-notab (integer->char 0)))

(begin
  (define slatex.make-raw-line (lambda () (make-vector 5)))
  (define slatex.=notab 4)
  (define slatex.=tab 3)
  (define slatex.=space 2)
  (define slatex.=char 1)
  (define slatex.=rtedge 0))

(define slatex.make-line
  (lambda ()
    (let ((l (slatex.make-raw-line)))
      (vector-set! l slatex.=rtedge 0)
      (vector-set!
       l
       slatex.=char
       (make-string slatex.*max-line-length* #\space))
      (vector-set!
       l
       slatex.=space
       (make-string slatex.*max-line-length* slatex.&void-space))
      (vector-set!
       l
       slatex.=tab
       (make-string slatex.*max-line-length* slatex.&void-tab))
      (vector-set!
       l
       slatex.=notab
       (make-string slatex.*max-line-length* slatex.&void-notab))
      l)))

(define slatex.*line1* (slatex.make-line))

(define slatex.*line2* (slatex.make-line))

(begin
  (define slatex.make-case-frame (lambda () (make-vector 3)))
  (define slatex.=in-case-exp 2)
  (define slatex.=in-bktd-ctag-exp 1)
  (define =in-ctag-tkn 0))

(begin
  (define slatex.make-bq-frame (lambda () (make-vector 3)))
  (define slatex.=in-bktd-bq-exp 2)
  (define slatex.=in-bq-tkn 1)
  (define slatex.=in-comma 0))

(define slatex.*latex-paragraph-mode?* 'fwd1)

(define slatex.*intext?* 'fwd2)

(define slatex.*code-env-spec* "UNDEFINED")

(define slatex.*in* 'fwd3)

(define slatex.*out* 'fwd4)

(define slatex.*in-qtd-tkn* 'fwd5)

(define slatex.*in-bktd-qtd-exp* 'fwd6)

(define slatex.*in-mac-tkn* 'fwd7)

(define slatex.*in-bktd-mac-exp* 'fwd8)

(define slatex.*case-stack* 'fwd9)

(define slatex.*bq-stack* 'fwd10)

(define slatex.display-space
  (lambda (s p)
    (cond ((eqv? s slatex.&plain-space) (display #\space p))
          ((eqv? s slatex.&init-plain-space) (display #\space p))
          ((eqv? s slatex.&init-space) (display "\\HL " p))
          ((eqv? s slatex.&paren-space) (display "\\PRN " p))
          ((eqv? s slatex.&bracket-space) (display "\\BKT " p))
          ((eqv? s slatex.&quote-space) (display "\\QUO " p))
          ((eqv? s slatex.&inner-space) (display "\\ " p)))))

(define slatex.display-tab
  (lambda (tab p)
    (cond ((eqv? tab slatex.&set-tab) (display "\\=" p))
          ((eqv? tab slatex.&move-tab) (display "\\>" p)))))

(define slatex.display-notab
  (lambda (notab p)
    (cond ((eqv? notab slatex.&begin-string) (display "\\dt{" p))
          ((eqv? notab slatex.&end-string) (display "}" p)))))

(define slatex.get-line
  (let ((curr-notab slatex.&void-notab))
    (lambda (line)
      (let ((graphic-char-seen? #f))
        (let loop ((i 0))
          (let ((c (read-char slatex.*in*)))
            (cond (graphic-char-seen? 'already-seen)
                  ((or (eof-object? c)
                       (char=? c slatex.*return*)
                       (char=? c #\newline)
                       (char=? c #\space)
                       (char=? c slatex.*tab*))
                   'not-yet)
                  (else (set! graphic-char-seen? #t)))
            (cond ((eof-object? c)
                   (cond ((eqv? curr-notab slatex.&mid-string)
                          (if (> i 0)
                              (string-set!
                               (vector-ref line slatex.=notab)
                               (- i 1)
                               slatex.&end-string)))
                         ((eqv? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eqv? curr-notab slatex.&mid-math)
                          (slatex.error
                           'slatex.get-line
                           'runaway-math-subformula)))
                   (string-set! (vector-ref line slatex.=char) i #\newline)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eqv? (string-ref (vector-ref line slatex.=notab) 0)
                             slatex.&mid-string)
                       (string-set!
                        (vector-ref line slatex.=notab)
                        0
                        slatex.&begin-string))
                   (if (= i 0) #f #t))
                  ((or (char=? c slatex.*return*) (char=? c #\newline))
                   (if (and (eq? *op-sys* 'dos) (char=? c slatex.*return*))
                       (if (char=? (peek-char slatex.*in*) #\newline)
                           (read-char slatex.*in*)))
                   (cond ((eqv? curr-notab slatex.&mid-string)
                          (if (> i 0)
                              (string-set!
                               (vector-ref line slatex.=notab)
                               (- i 1)
                               slatex.&end-string)))
                         ((eqv? curr-notab slatex.&mid-comment)
                          (set! curr-notab slatex.&void-notab))
                         ((eqv? curr-notab slatex.&mid-math)
                          (slatex.error
                           'slatex.get-line
                           'runaway-math-subformula)))
                   (string-set! (vector-ref line slatex.=char) i #\newline)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    (cond ((eof-object? (peek-char slatex.*in*))
                           slatex.&plain-crg-ret)
                          (slatex.*intext?* slatex.&plain-crg-ret)
                          (else slatex.&tabbed-crg-ret)))
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&void-notab)
                   (vector-set! line slatex.=rtedge i)
                   (if (eqv? (string-ref (vector-ref line slatex.=notab) 0)
                             slatex.&mid-string)
                       (string-set!
                        (vector-ref line slatex.=notab)
                        0
                        slatex.&begin-string))
                   #t)
                  ((eqv? curr-notab slatex.&mid-comment)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    (cond ((char=? c #\space) slatex.&plain-space)
                          ((char=? c slatex.*tab*) slatex.&plain-space)
                          (else slatex.&void-space)))
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&mid-comment)
                   (loop (+ i 1)))
                  ((char=? c #\\)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set! (vector-ref line slatex.=notab) i curr-notab)
                   (let ((i+1 (+ i 1)) (c+1 (read-char slatex.*in*)))
                     (if (char=? c+1 slatex.*tab*) (set! c+1 #\space))
                     (string-set! (vector-ref line slatex.=char) i+1 c+1)
                     (string-set!
                      (vector-ref line slatex.=space)
                      i+1
                      (if (char=? c+1 #\space)
                          slatex.&plain-space
                          slatex.&void-space))
                     (string-set!
                      (vector-ref line slatex.=tab)
                      i+1
                      slatex.&void-tab)
                     (string-set!
                      (vector-ref line slatex.=notab)
                      i+1
                      curr-notab)
                     (loop (+ i+1 1))))
                  ((eqv? curr-notab slatex.&mid-math)
                   (if (char=? c slatex.*tab*) (set! c #\space))
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    (if (char=? c #\space)
                        slatex.&plain-space
                        slatex.&void-space))
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (cond ((memv c slatex.*math-triggerers*)
                          (string-set! (vector-ref line slatex.=char) i #\$)
                          (string-set!
                           (vector-ref line slatex.=notab)
                           i
                           slatex.&end-math)
                          (set! curr-notab slatex.&void-notab))
                         (else
                          (string-set! (vector-ref line slatex.=char) i c)
                          (string-set!
                           (vector-ref line slatex.=notab)
                           i
                           slatex.&mid-math)))
                   (loop (+ i 1)))
                  ((eqv? curr-notab slatex.&mid-string)
                   (if (char=? c slatex.*tab*) (set! c #\space))
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    (if (char=? c #\space)
                        slatex.&inner-space
                        slatex.&void-space))
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    (cond ((char=? c #\")
                           (set! curr-notab slatex.&void-notab)
                           slatex.&end-string)
                          (else slatex.&mid-string)))
                   (loop (+ i 1)))
                  ((char=? c #\space)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    (cond (slatex.*intext?* slatex.&plain-space)
                          (graphic-char-seen? slatex.&inner-space)
                          (else slatex.&init-space)))
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&void-notab)
                   (loop (+ i 1)))
                  ((char=? c slatex.*tab*)
                   (let loop2 ((i i) (j 0))
                     (if (< j 8)
                         (begin
                           (string-set! (vector-ref line slatex.=char) i #\space)
                           (string-set!
                            (vector-ref line slatex.=space)
                            i
                            (cond (slatex.*intext?* slatex.&plain-space)
                                  (graphic-char-seen? slatex.&inner-space)
                                  (else slatex.&init-space)))
                           (string-set!
                            (vector-ref line slatex.=tab)
                            i
                            slatex.&void-tab)
                           (string-set!
                            (vector-ref line slatex.=notab)
                            i
                            slatex.&void-notab)
                           (loop2 (+ i 1) (+ j 1)))))
                   (loop (+ i 8)))
                  ((char=? c #\")
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&begin-string)
                   (set! curr-notab slatex.&mid-string)
                   (loop (+ i 1)))
                  ((char=? c #\;)
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&begin-comment)
                   (set! curr-notab slatex.&mid-comment)
                   (loop (+ i 1)))
                  ((memv c slatex.*math-triggerers*)
                   (string-set! (vector-ref line slatex.=char) i #\$)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&begin-math)
                   (set! curr-notab slatex.&mid-math)
                   (loop (+ i 1)))
                  (else
                   (string-set! (vector-ref line slatex.=char) i c)
                   (string-set!
                    (vector-ref line slatex.=space)
                    i
                    slatex.&void-space)
                   (string-set!
                    (vector-ref line slatex.=tab)
                    i
                    slatex.&void-tab)
                   (string-set!
                    (vector-ref line slatex.=notab)
                    i
                    slatex.&void-notab)
                   (loop (+ i 1))))))))))

(define slatex.peephole-adjust
  (lambda (curr prev)
    (if (or (slatex.blank-line? curr) (slatex.flush-comment-line? curr))
        (if slatex.*latex-paragraph-mode?*
            'skip
            (begin
              (set! slatex.*latex-paragraph-mode?* #t)
              (if slatex.*intext?*
                  'skip
                  (begin
                    (slatex.remove-some-tabs prev 0)
                    (let ((prev-rtedge (vector-ref prev slatex.=rtedge)))
                      (if (eqv? (string-ref (vector-ref prev slatex.=tab)
                                            prev-rtedge)
                                slatex.&tabbed-crg-ret)
                          (string-set!
                           (vector-ref prev slatex.=tab)
                           (vector-ref prev slatex.=rtedge)
                           slatex.&plain-crg-ret)))))))
        (begin
          (if slatex.*latex-paragraph-mode?*
              (set! slatex.*latex-paragraph-mode?* #f)
              (if slatex.*intext?*
                  'skip
                  (let ((remove-tabs-from #f))
                    (let loop ((i 0))
                      (cond ((char=? (string-ref (vector-ref curr slatex.=char) i)
                                     #\newline)
                             (set! remove-tabs-from i))
                            ((char=? (string-ref (vector-ref prev slatex.=char) i)
                                     #\newline)
                             (set! remove-tabs-from #f))
                            ((eqv? (string-ref (vector-ref curr slatex.=space) i)
                                   slatex.&init-space)
                             (if (eqv? (string-ref (vector-ref prev slatex.=notab) i)
                                       slatex.&void-notab)
                                 (begin
                                   (cond ((or (char=? (string-ref
                                                       (vector-ref prev slatex.=char)
                                                       i)
                                                      #\()
                                              (eqv? (string-ref
                                                     (vector-ref prev slatex.=space)
                                                     i)
                                                    slatex.&paren-space))
                                          (string-set!
                                           (vector-ref curr slatex.=space)
                                           i
                                           slatex.&paren-space))
                                         ((or (char=? (string-ref
                                                       (vector-ref prev slatex.=char)
                                                       i)
                                                      #\[)
                                              (eqv? (string-ref
                                                     (vector-ref prev slatex.=space)
                                                     i)
                                                    slatex.&bracket-space))
                                          (string-set!
                                           (vector-ref curr slatex.=space)
                                           i
                                           slatex.&bracket-space))
                                         ((or (memv (string-ref
                                                     (vector-ref prev slatex.=char)
                                                     i)
                                                    '(#\' #\` #\,))
                                              (eqv? (string-ref
                                                     (vector-ref prev slatex.=space)
                                                     i)
                                                    slatex.&quote-space))
                                          (string-set!
                                           (vector-ref curr slatex.=space)
                                           i
                                           slatex.&quote-space)))
                                   (if (memv (string-ref
                                              (vector-ref prev slatex.=tab)
                                              i)
                                             (list slatex.&set-tab slatex.&move-tab))
                                       (string-set!
                                        (vector-ref curr slatex.=tab)
                                        i
                                        slatex.&move-tab))))
                             (loop (+ i 1)))
                            ((= i 0) (set! remove-tabs-from 0))
                            ((not (eqv? (string-ref (vector-ref prev slatex.=tab) i)
                                        slatex.&void-tab))
                             (set! remove-tabs-from (+ i 1))
                             (if (memv (string-ref (vector-ref prev slatex.=tab) i)
                                       (list slatex.&set-tab slatex.&move-tab))
                                 (string-set!
                                  (vector-ref curr slatex.=tab)
                                  i
                                  slatex.&move-tab)))
                            ((memv (string-ref (vector-ref prev slatex.=space) i)
                                   (list slatex.&init-space
                                         slatex.&init-plain-space
                                         slatex.&paren-space
                                         slatex.&bracket-space
                                         slatex.&quote-space))
                             (set! remove-tabs-from (+ i 1)))
                            ((and (char=? (string-ref
                                           (vector-ref prev slatex.=char)
                                           (- i 1))
                                          #\space)
                                  (eqv? (string-ref
                                         (vector-ref prev slatex.=notab)
                                         (- i 1))
                                        slatex.&void-notab))
                             (set! remove-tabs-from (+ i 1))
                             (string-set!
                              (vector-ref prev slatex.=tab)
                              i
                              slatex.&set-tab)
                             (string-set!
                              (vector-ref curr slatex.=tab)
                              i
                              slatex.&move-tab))
                            (else
                             (set! remove-tabs-from (+ i 1))
                             (let loop1 ((j (- i 1)))
                               (cond ((<= j 0) 'exit-loop1)
                                     ((not (eqv? (string-ref
                                                  (vector-ref curr slatex.=tab)
                                                  j)
                                                 slatex.&void-tab))
                                      'exit-loop1)
                                     ((memv (string-ref
                                             (vector-ref curr slatex.=space)
                                             j)
                                            (list slatex.&paren-space
                                                  slatex.&bracket-space
                                                  slatex.&quote-space))
                                      (loop1 (- j 1)))
                                     ((or (not (eqv? (string-ref
                                                      (vector-ref prev
                                                                  slatex.=notab)
                                                      j)
                                                     slatex.&void-notab))
                                          (char=? (string-ref
                                                   (vector-ref prev slatex.=char)
                                                   j)
                                                  #\space))
                                      (let ((k (+ j 1)))
                                        (if (memv (string-ref
                                                   (vector-ref prev slatex.=notab)
                                                   k)
                                                  (list slatex.&mid-comment
                                                        slatex.&mid-math
                                                        slatex.&end-math
                                                        slatex.&mid-string
                                                        slatex.&end-string))
                                            'skip
                                            (begin
                                              (if (eqv? (string-ref
                                                         (vector-ref prev slatex.=tab)
                                                         k)
                                                        slatex.&void-tab)
                                                  (string-set!
                                                   (vector-ref prev slatex.=tab)
                                                   k
                                                   slatex.&set-tab))
                                              (string-set!
                                               (vector-ref curr slatex.=tab)
                                               k
                                               slatex.&move-tab)))))
                                     (else 'anything-else?))))))
                    (slatex.remove-some-tabs prev remove-tabs-from))))
          (if slatex.*intext?* 'skip (slatex.add-some-tabs curr))
          (slatex.clean-init-spaces curr)
          (slatex.clean-inner-spaces curr)))))

(define slatex.add-some-tabs
  (lambda (line)
    (let loop ((i 1) (succ-parens? #f))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\newline) 'exit-loop)
              ((not (eqv? (string-ref (vector-ref line slatex.=notab) i)
                          slatex.&void-notab))
               (loop (+ i 1) #f))
              ((char=? c #\[)
               (if (eqv? (string-ref (vector-ref line slatex.=tab) i)
                         slatex.&void-tab)
                   (string-set! (vector-ref line slatex.=tab) i slatex.&set-tab))
               (loop (+ i 1) #f))
              ((char=? c #\()
               (if (eqv? (string-ref (vector-ref line slatex.=tab) i)
                         slatex.&void-tab)
                   (if succ-parens?
                       'skip
                       (string-set!
                        (vector-ref line slatex.=tab)
                        i
                        slatex.&set-tab)))
               (loop (+ i 1) #t))
              (else (loop (+ i 1) #f)))))))

(define slatex.remove-some-tabs
  (lambda (line i)
    (if i
        (let loop ((i i))
          (cond ((char=? (string-ref (vector-ref line slatex.=char) i) #\newline)
                 'exit)
                ((eqv? (string-ref (vector-ref line slatex.=tab) i)
                       slatex.&set-tab)
                 (string-set! (vector-ref line slatex.=tab) i slatex.&void-tab)
                 (loop (+ i 1)))
                (else (loop (+ i 1))))))))

(define slatex.clean-init-spaces
  (lambda (line)
    (let loop ((i (vector-ref line slatex.=rtedge)))
      (cond ((< i 0) 'exit-loop)
            ((eqv? (string-ref (vector-ref line slatex.=tab) i)
                   slatex.&move-tab)
             (let loop2 ((i (- i 1)))
               (cond ((< i 0) 'exit-loop2)
                     ((memv (string-ref (vector-ref line slatex.=space) i)
                            (list slatex.&init-space
                                  slatex.&paren-space
                                  slatex.&bracket-space
                                  slatex.&quote-space))
                      (string-set!
                       (vector-ref line slatex.=space)
                       i
                       slatex.&init-plain-space)
                      (loop2 (- i 1)))
                     (else (loop2 (- i 1))))))
            (else (loop (- i 1)))))))

(define slatex.clean-inner-spaces
  (lambda (line)
    (let loop ((i 0) (succ-inner-spaces? #f))
      (cond ((char=? (string-ref (vector-ref line slatex.=char) i) #\newline)
             'exit-loop)
            ((eqv? (string-ref (vector-ref line slatex.=space) i)
                   slatex.&inner-space)
             (if succ-inner-spaces?
                 'skip
                 (string-set!
                  (vector-ref line slatex.=space)
                  i
                  slatex.&plain-space))
             (loop (+ i 1) #t))
            (else (loop (+ i 1) #f))))))

(define slatex.blank-line?
  (lambda (line)
    (let loop ((i 0))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\space)
               (if (eqv? (string-ref (vector-ref line slatex.=notab) i)
                         slatex.&void-notab)
                   (loop (+ i 1))
                   #f))
              ((char=? c #\newline)
               (let loop2 ((j (- i 1)))
                 (if (<= j 0)
                     'skip
                     (begin
                       (string-set!
                        (vector-ref line slatex.=space)
                        i
                        slatex.&void-space)
                       (loop2 (- j 1)))))
               #t)
              (else #f))))))

(define slatex.flush-comment-line?
  (lambda (line)
    (and (char=? (string-ref (vector-ref line slatex.=char) 0) #\;)
         (eqv? (string-ref (vector-ref line slatex.=notab) 0)
               slatex.&begin-comment)
         (not (char=? (string-ref (vector-ref line slatex.=char) 1) #\;)))))

(define slatex.do-all-lines
  (lambda ()
    (let loop ((line1 slatex.*line1*) (line2 slatex.*line2*))
      (let* ((line2-paragraph? slatex.*latex-paragraph-mode?*)
             (more? (slatex.get-line line1)))
        (slatex.peephole-adjust line1 line2)
        ((if line2-paragraph? slatex.display-tex-line slatex.display-scm-line)
         line2)
        (if (eq? line2-paragraph? slatex.*latex-paragraph-mode?*)
            'else
            ((if slatex.*latex-paragraph-mode?*
                 slatex.display-end-sequence
                 slatex.display-begin-sequence)
             slatex.*out*))
        (if more? (loop line2 line1))))))

(define scheme2tex
  (lambda (inport outport)
    (set! slatex.*in* inport)
    (set! slatex.*out* outport)
    (set! slatex.*latex-paragraph-mode?* #t)
    (set! slatex.*in-qtd-tkn* #f)
    (set! slatex.*in-bktd-qtd-exp* 0)
    (set! slatex.*in-mac-tkn* #f)
    (set! slatex.*in-bktd-mac-exp* 0)
    (set! slatex.*case-stack* '())
    (set! slatex.*bq-stack* '())
    (let ((flush-line
           (lambda (line)
             (vector-set! line slatex.=rtedge 0)
             (string-set! (vector-ref line slatex.=char) 0 #\newline)
             (string-set!
              (vector-ref line slatex.=space)
              0
              slatex.&void-space)
             (string-set! (vector-ref line slatex.=tab) 0 slatex.&void-tab)
             (string-set!
              (vector-ref line slatex.=notab)
              0
              slatex.&void-notab))))
      (flush-line slatex.*line1*)
      (flush-line slatex.*line2*))
    (slatex.do-all-lines)))

(define slatex.display-tex-line
  (lambda (line)
    (cond (else
           (let loop ((i (if (slatex.flush-comment-line? line) 1 0)))
             (let ((c (string-ref (vector-ref line slatex.=char) i)))
               (if (char=? c #\newline)
                   (if (eqv? (string-ref (vector-ref line slatex.=tab) i)
                             slatex.&void-tab)
                       'skip
                       (newline slatex.*out*))
                   (begin (display c slatex.*out*) (loop (+ i 1))))))))))

(define slatex.display-scm-line
  (lambda (line)
    (let loop ((i 0))
      (let ((c (string-ref (vector-ref line slatex.=char) i)))
        (cond ((char=? c #\newline)
               (let ((tab (string-ref (vector-ref line slatex.=tab) i)))
                 (cond ((eqv? tab slatex.&tabbed-crg-ret)
                        (display "\\\\" slatex.*out*)
                        (newline slatex.*out*))
                       ((eqv? tab slatex.&plain-crg-ret)
                        (newline slatex.*out*))
                       ((eqv? tab slatex.&void-tab)
                        (display #\% slatex.*out*)
                        (newline slatex.*out*)))))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-comment)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (loop (+ i 1)))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&mid-comment)
               (display c slatex.*out*)
               (loop (+ i 1)))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-string)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display "\\dt{" slatex.*out*)
               (if (char=? c #\space)
                   (slatex.display-space
                    (string-ref (vector-ref line slatex.=space) i)
                    slatex.*out*)
                   (slatex.display-tex-char c slatex.*out*))
               (loop (+ i 1)))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&mid-string)
               (if (char=? c #\space)
                   (slatex.display-space
                    (string-ref (vector-ref line slatex.=space) i)
                    slatex.*out*)
                   (slatex.display-tex-char c slatex.*out*))
               (loop (+ i 1)))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&end-string)
               (if (char=? c #\space)
                   (slatex.display-space
                    (string-ref (vector-ref line slatex.=space) i)
                    slatex.*out*)
                   (slatex.display-tex-char c slatex.*out*))
               (display "}" slatex.*out*)
               (loop (+ i 1)))
              ((eqv? (string-ref (vector-ref line slatex.=notab) i)
                     slatex.&begin-math)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (loop (+ i 1)))
              ((memv (string-ref (vector-ref line slatex.=notab) i)
                     (list slatex.&mid-math slatex.&end-math))
               (display c slatex.*out*)
               (loop (+ i 1)))
              ((char=? c #\space)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (slatex.display-space
                (string-ref (vector-ref line slatex.=space) i)
                slatex.*out*)
               (loop (+ i 1)))
              ((char=? c #\')
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (if (or slatex.*in-qtd-tkn* (> slatex.*in-bktd-qtd-exp* 0))
                   'skip
                   (set! slatex.*in-qtd-tkn* #t))
               (loop (+ i 1)))
              ((char=? c #\`)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref (car slatex.*bq-stack*) slatex.=in-comma))
                   (set! slatex.*bq-stack*
                         (cons (let ((f (slatex.make-bq-frame)))
                                 (vector-set! f slatex.=in-comma #f)
                                 (vector-set! f slatex.=in-bq-tkn #t)
                                 (vector-set! f slatex.=in-bktd-bq-exp 0)
                                 f)
                               slatex.*bq-stack*)))
               (loop (+ i 1)))
              ((char=? c #\,)
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (if (or (null? slatex.*bq-stack*)
                       (vector-ref (car slatex.*bq-stack*) slatex.=in-comma))
                   'skip
                   (set! slatex.*bq-stack*
                         (cons (let ((f (slatex.make-bq-frame)))
                                 (vector-set! f slatex.=in-comma #t)
                                 (vector-set! f slatex.=in-bq-tkn #t)
                                 (vector-set! f slatex.=in-bktd-bq-exp 0)
                                 f)
                               slatex.*bq-stack*)))
               (if (char=? (string-ref (vector-ref line slatex.=char) (+ i 1))
                           #\@)
                   (begin
                     (slatex.display-tex-char #\@ slatex.*out*)
                     (loop (+ 2 i)))
                   (loop (+ i 1))))
              ((memv c '(#\( #\[))
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (cond (slatex.*in-qtd-tkn*
                      (set! slatex.*in-qtd-tkn* #f)
                      (set! slatex.*in-bktd-qtd-exp* 1))
                     ((> slatex.*in-bktd-qtd-exp* 0)
                      (set! slatex.*in-bktd-qtd-exp*
                            (+ slatex.*in-bktd-qtd-exp* 1))))
               (cond (slatex.*in-mac-tkn*
                      (set! slatex.*in-mac-tkn* #f)
                      (set! slatex.*in-bktd-mac-exp* 1))
                     ((> slatex.*in-bktd-mac-exp* 0)
                      (set! slatex.*in-bktd-mac-exp*
                            (+ slatex.*in-bktd-mac-exp* 1))))
               (if (null? slatex.*bq-stack*)
                   'skip
                   (let ((top (car slatex.*bq-stack*)))
                     (cond ((vector-ref top slatex.=in-bq-tkn)
                            (vector-set! top slatex.=in-bq-tkn #f)
                            (vector-set! top slatex.=in-bktd-bq-exp 1))
                           ((> (vector-ref top slatex.=in-bktd-bq-exp) 0)
                            (vector-set!
                             top
                             slatex.=in-bktd-bq-exp
                             (+ (vector-ref top slatex.=in-bktd-bq-exp) 1))))))
               (if (null? slatex.*case-stack*)
                   'skip
                   (let ((top (car slatex.*case-stack*)))
                     (cond ((vector-ref top =in-ctag-tkn)
                            (vector-set! top =in-ctag-tkn #f)
                            (vector-set! top slatex.=in-bktd-ctag-exp 1))
                           ((> (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                            (vector-set!
                             top
                             slatex.=in-bktd-ctag-exp
                             (+ (vector-ref top slatex.=in-bktd-ctag-exp) 1)))
                           ((> (vector-ref top slatex.=in-case-exp) 0)
                            (vector-set!
                             top
                             slatex.=in-case-exp
                             (+ (vector-ref top slatex.=in-case-exp) 1))
                            (if (= (vector-ref top slatex.=in-case-exp) 2)
                                (set! slatex.*in-qtd-tkn* #t))))))
               (loop (+ i 1)))
              ((memv c '(#\) #\]))
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (display c slatex.*out*)
               (if (> slatex.*in-bktd-qtd-exp* 0)
                   (set! slatex.*in-bktd-qtd-exp*
                         (- slatex.*in-bktd-qtd-exp* 1)))
               (if (> slatex.*in-bktd-mac-exp* 0)
                   (set! slatex.*in-bktd-mac-exp*
                         (- slatex.*in-bktd-mac-exp* 1)))
               (if (null? slatex.*bq-stack*)
                   'skip
                   (let ((top (car slatex.*bq-stack*)))
                     (if (> (vector-ref top slatex.=in-bktd-bq-exp) 0)
                         (begin
                           (vector-set!
                            top
                            slatex.=in-bktd-bq-exp
                            (- (vector-ref top slatex.=in-bktd-bq-exp) 1))
                           (if (= (vector-ref top slatex.=in-bktd-bq-exp) 0)
                               (set! slatex.*bq-stack* (cdr slatex.*bq-stack*)))))))
               (let loop ()
                 (if (null? slatex.*case-stack*)
                     'skip
                     (let ((top (car slatex.*case-stack*)))
                       (cond ((> (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                              (vector-set!
                               top
                               slatex.=in-bktd-ctag-exp
                               (- (vector-ref top slatex.=in-bktd-ctag-exp) 1))
                              (if (= (vector-ref top slatex.=in-bktd-ctag-exp) 0)
                                  (vector-set! top slatex.=in-case-exp 1)))
                             ((> (vector-ref top slatex.=in-case-exp) 0)
                              (vector-set!
                               top
                               slatex.=in-case-exp
                               (- (vector-ref top slatex.=in-case-exp) 1))
                              (if (= (vector-ref top slatex.=in-case-exp) 0)
                                  (begin
                                    (set! slatex.*case-stack*
                                          (cdr slatex.*case-stack*))
                                    (loop))))))))
               (loop (+ i 1)))
              (else
               (slatex.display-tab
                (string-ref (vector-ref line slatex.=tab) i)
                slatex.*out*)
               (loop (slatex.do-token line i))))))))

(define slatex.do-token
  (let ((token-delims
         (list #\(
               #\)
               #\[
               #\]
               #\space
               slatex.*return*
               #\newline
               #\,
               #\@
               #\;)))
    (lambda (line i)
      (let loop ((buf '()) (i i))
        (let ((c (string-ref (vector-ref line slatex.=char) i)))
          (cond ((char=? c #\\)
                 (loop (cons (string-ref
                              (vector-ref line slatex.=char)
                              (+ i 1))
                             (cons c buf))
                       (+ i 2)))
                ((or (memv c token-delims) (memv c slatex.*math-triggerers*))
                 (slatex.output-token (list->string (slatex.reverse! buf)))
                 i)
                ((char? c)
                 (loop (cons (string-ref (vector-ref line slatex.=char) i) buf)
                       (+ i 1)))
                (else (slatex.error 'slatex.do-token 1))))))))

(define slatex.output-token
  (lambda (token)
    (if (null? slatex.*case-stack*)
        'skip
        (let ((top (car slatex.*case-stack*)))
          (if (vector-ref top =in-ctag-tkn)
              (begin
                (vector-set! top =in-ctag-tkn #f)
                (vector-set! top slatex.=in-case-exp 1)))))
    (if (slatex.assoc-token token slatex.special-symbols)
        (display (cdr (slatex.assoc-token token slatex.special-symbols))
                 slatex.*out*)
        (slatex.display-token
         token
         (cond (slatex.*in-qtd-tkn*
                (set! slatex.*in-qtd-tkn* #f)
                (cond ((equal? token "else") 'syntax)
                      ((slatex.data-token? token) 'data)
                      (else 'constant)))
               ((slatex.data-token? token) 'data)
               ((> slatex.*in-bktd-qtd-exp* 0) 'constant)
               ((and (not (null? slatex.*bq-stack*))
                     (not (vector-ref
                           (car slatex.*bq-stack*)
                           slatex.=in-comma)))
                'constant)
               (slatex.*in-mac-tkn*
                (set! slatex.*in-mac-tkn* #f)
                (slatex.set-keyword token)
                'syntax)
               ((> slatex.*in-bktd-mac-exp* 0)
                (slatex.set-keyword token)
                'syntax)
               ((slatex.member-token token slatex.constant-tokens) 'constant)
               ((slatex.member-token token slatex.variable-tokens) 'variable)
               ((slatex.member-token token slatex.keyword-tokens)
                (cond ((slatex.token=? token "quote")
                       (set! slatex.*in-qtd-tkn* #t))
                      ((slatex.member-token token slatex.macro-definers)
                       (set! slatex.*in-mac-tkn* #t))
                      ((slatex.member-token token slatex.case-and-ilk)
                       (set! slatex.*case-stack*
                             (cons (let ((f (slatex.make-case-frame)))
                                     (vector-set! f =in-ctag-tkn #t)
                                     (vector-set! f slatex.=in-bktd-ctag-exp 0)
                                     (vector-set! f slatex.=in-case-exp 0)
                                     f)
                                   slatex.*case-stack*))))
                'syntax)
               (else 'variable))
         slatex.*out*))
    (if (and (not (null? slatex.*bq-stack*))
             (vector-ref (car slatex.*bq-stack*) slatex.=in-bq-tkn))
        (set! slatex.*bq-stack* (cdr slatex.*bq-stack*)))))

(define slatex.data-token?
  (lambda (token)
    (or (char=? (string-ref token 0) #\#) (string->number token))))

(define slatex.*texinputs* "")

(define slatex.*texinputs-list* '())

(define slatex.*path-separator*
  (cond ((eq? *op-sys* 'unix) #\:)
        ((eq? *op-sys* 'dos) #\;)
        (else (slatex.error 'slatex.*path-separator* 'cant-determine))))

(define slatex.*directory-mark*
  (cond ((eq? *op-sys* 'unix) "/")
        ((eq? *op-sys* 'dos) "\\")
        (else (slatex.error 'slatex.*directory-mark* 'cant-determine))))

;; Relative to the working directory, which for these ports is the repo root.
(define slatex.*file-hider*
  (cond ((eq? *op-sys* 'unix) "r7rs-benchmarks/outputs/")
        ((eq? *op-sys* 'dos) "x")
        (else ".")))

(define slatex.path->list
  (lambda (p)
    (let loop ((p (string->list p)) (r (list "")))
      (let ((separator-pos (slatex.position-char slatex.*path-separator* p)))
        (if separator-pos
            (loop (list-tail p (+ separator-pos 1))
                  (cons (list->string (slatex.sublist p 0 separator-pos)) r))
            (slatex.reverse! (cons (list->string p) r)))))))

(define slatex.find-some-file
  (lambda (path . files)
    (let loop ((path path))
      (if (null? path)
          #f
          (let ((dir (car path)))
            (let loop2 ((files (if (or (string=? dir "") (string=? dir "."))
                                   files
                                   (map (lambda (file)
                                          (string-append
                                           dir
                                           slatex.*directory-mark*
                                           file))
                                        files))))
              (if (null? files)
                  (loop (cdr path))
                  (let ((file (car files)))
                    (if (slatex.file-exists? file)
                        file
                        (loop2 (cdr files)))))))))))

(define slatex.file-extension
  (lambda (filename)
    (let ((i (slatex.string-position-right #\. filename)))
      (if i (substring filename i (string-length filename)) #f))))

(define slatex.basename
  (lambda (filename ext)
    (let* ((filename-len (string-length filename))
           (ext-len (string-length ext))
           (len-diff (- filename-len ext-len)))
      (cond ((> ext-len filename-len) filename)
            ((equal? ext (substring filename len-diff filename-len))
             (substring filename 0 len-diff))
            (else filename)))))

(define slatex.full-texfile-name
  (lambda (filename)
    (let ((extn (slatex.file-extension filename)))
      (if (and extn (or (string=? extn ".sty") (string=? extn ".tex")))
          (slatex.find-some-file slatex.*texinputs-list* filename)
          (slatex.find-some-file
           slatex.*texinputs-list*
           (string-append filename ".tex")
           filename)))))

(define slatex.full-scmfile-name
  (lambda (filename)
    (apply slatex.find-some-file
           slatex.*texinputs-list*
           filename
           (map (lambda (extn) (string-append filename extn))
                '(".scm" ".ss" ".s")))))

(define slatex.new-aux-file
  (lambda e
    (apply (if slatex.*slatex-in-protected-region?*
               slatex.new-secondary-aux-file
               slatex.new-primary-aux-file)
           e)))

(define slatex.subjobname 'fwd)

(define primary-aux-file-count -1)

(define slatex.new-primary-aux-file
  (lambda e
    (set! primary-aux-file-count (+ primary-aux-file-count 1))
    (apply string-append
           slatex.*file-hider*
           "z"
           (number->string primary-aux-file-count)
                                        ;           slatex.subjobname
           e)))

(define slatex.new-secondary-aux-file
  (let ((n -1))
    (lambda e
      (set! n (+ n 1))
      (apply string-append
             slatex.*file-hider*
             "zz"
             (number->string n)
                                        ;             slatex.subjobname
             e))))

(define slatex.eat-till-newline
  (lambda (in)
    (let loop ()
      (let ((c (read-char in)))
        (cond ((eof-object? c) 'done)
              ((char=? c #\newline) 'done)
              (else (loop)))))))

(define slatex.read-ctrl-seq
  (lambda (in)
    (let ((c (read-char in)))
      (if (eof-object? c) (slatex.error 'read-ctrl-exp 1))
      (if (char-alphabetic? c)
          (list->string
           (slatex.reverse!
            (let loop ((s (list c)))
              (let ((c (peek-char in)))
                (cond ((eof-object? c) s)
                      ((char-alphabetic? c) (read-char in) (loop (cons c s)))
                      ((char=? c #\%) (slatex.eat-till-newline in) (loop s))
                      (else s))))))
          (string c)))))

(define slatex.eat-tabspace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((or (char=? c #\space) (char=? c slatex.*tab*))
               (read-char in)
               (loop))
              (else 'done))))))

(define slatex.eat-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((char-whitespace? c) (read-char in) (loop))
              (else 'done))))))

(define slatex.eat-latex-whitespace
  (lambda (in)
    (let loop ()
      (let ((c (peek-char in)))
        (cond ((eof-object? c) 'done)
              ((char-whitespace? c) (read-char in) (loop))
              ((char=? c #\%) (slatex.eat-till-newline in))
              (else 'done))))))

(define slatex.chop-off-whitespace
  (lambda (l)
    (slatex.ormapcdr (lambda (d) (if (char-whitespace? (car d)) #f d)) l)))

(define slatex.read-grouped-latexexp
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c) (slatex.error 'slatex.read-grouped-latexexp 1))
      (if (char=? c #\{) 'ok (slatex.error 'slatex.read-grouped-latexexp 2))
      (slatex.eat-latex-whitespace in)
      (list->string
       (slatex.reverse!
        (slatex.chop-off-whitespace
         (let loop ((s '()) (nesting 0) (escape? #f))
           (let ((c (read-char in)))
             (if (eof-object? c)
                 (slatex.error 'slatex.read-grouped-latexexp 3))
             (cond (escape? (loop (cons c s) nesting #f))
                   ((char=? c #\\) (loop (cons c s) nesting #t))
                   ((char=? c #\%)
                    (slatex.eat-till-newline in)
                    (loop s nesting #f))
                   ((char=? c #\{) (loop (cons c s) (+ nesting 1) #f))
                   ((char=? c #\})
                    (if (= nesting 0) s (loop (cons c s) (- nesting 1) #f)))
                   (else (loop (cons c s) nesting #f)))))))))))

(define slatex.read-filename
  (let ((filename-delims
         (list #\{
               #\}
               #\[
               #\]
               #\(
               #\)
               #\#
               #\%
               #\\
               #\,
               #\space
               slatex.*return*
               #\newline
               slatex.*tab*)))
    (lambda (in)
      (slatex.eat-latex-whitespace in)
      (let ((c (peek-char in)))
        (if (eof-object? c) (slatex.error 'slatex.read-filename 1))
        (if (char=? c #\{)
            (slatex.read-grouped-latexexp in)
            (list->string
             (slatex.reverse!
              (let loop ((s '()) (escape? #f))
                (let ((c (peek-char in)))
                  (cond ((eof-object? c)
                         (if escape? (slatex.error 'slatex.read-filename 2) s))
                        (escape? (read-char in) (loop (cons c s) #f))
                        ((char=? c #\\) (read-char in) (loop (cons c s) #t))
                        ((memv c filename-delims) s)
                        (else (read-char in) (loop (cons c s) #f))))))))))))

(define slatex.read-schemeid
  (let ((schemeid-delims
         (list #\{
               #\}
               #\[
               #\]
               #\(
               #\)
               #\space
               slatex.*return*
               #\newline
               slatex.*tab*)))
    (lambda (in)
      (slatex.eat-whitespace in)
      (list->string
       (slatex.reverse!
        (let loop ((s '()) (escape? #f))
          (let ((c (peek-char in)))
            (cond ((eof-object? c) s)
                  (escape? (read-char in) (loop (cons c s) #f))
                  ((char=? c #\\) (read-char in) (loop (cons c s) #t))
                  ((memv c schemeid-delims) s)
                  (else (read-char in) (loop (cons c s) #f))))))))))

(define slatex.read-delimed-commaed-filenames
  (lambda (in lft-delim rt-delim)
    (slatex.eat-latex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c)
          (slatex.error 'slatex.read-delimed-commaed-filenames 1))
      (if (char=? c lft-delim)
          'ok
          (slatex.error 'slatex.read-delimed-commaed-filenames 2))
      (let loop ((s '()))
        (slatex.eat-latex-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c)
              (slatex.error 'slatex.read-delimed-commaed-filenames 3))
          (if (char=? c rt-delim)
              (begin (read-char in) (slatex.reverse! s))
              (let ((s (cons (slatex.read-filename in) s)))
                (slatex.eat-latex-whitespace in)
                (let ((c (peek-char in)))
                  (if (eof-object? c)
                      (slatex.error 'slatex.read-delimed-commaed-filenames 4))
                  (cond ((char=? c #\,) (read-char in))
                        ((char=? c rt-delim) 'void)
                        (else
                         (slatex.error
                          'slatex.read-delimed-commaed-filenames
                          5)))
                  (loop s)))))))))

(define slatex.read-grouped-commaed-filenames
  (lambda (in) (slatex.read-delimed-commaed-filenames in #\{ #\})))

(define slatex.read-bktd-commaed-filenames
  (lambda (in) (slatex.read-delimed-commaed-filenames in #\[ #\])))

(define slatex.read-grouped-schemeids
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (let ((c (read-char in)))
      (if (eof-object? c) (slatex.error 'slatex.read-grouped-schemeids 1))
      (if (char=? c #\{) 'ok (slatex.error 'slatex.read-grouped-schemeids 2))
      (let loop ((s '()))
        (slatex.eat-whitespace in)
        (let ((c (peek-char in)))
          (if (eof-object? c) (slatex.error 'slatex.read-grouped-schemeids 3))
          (if (char=? c #\})
              (begin (read-char in) (slatex.reverse! s))
              (loop (cons (slatex.read-schemeid in) s))))))))

(define slatex.disable-slatex-temply
  (lambda (in)
    (set! slatex.*slatex-enabled?* #f)
    (set! slatex.*slatex-reenabler* (slatex.read-grouped-latexexp in))))

(define slatex.enable-slatex-again
  (lambda ()
    (set! slatex.*slatex-enabled?* #t)
    (set! slatex.*slatex-reenabler* "UNDEFINED")))

(define slatex.ignore2 (lambda (i ii) 'void))

(define slatex.add-to-slatex-db
  (lambda (in categ)
    (if (memq categ '(keyword constant variable))
        (slatex.add-to-slatex-db-basic in categ)
        (slatex.add-to-slatex-db-special in categ))))

(define slatex.add-to-slatex-db-basic
  (lambda (in categ)
    (let ((setter (cond ((eq? categ 'keyword) slatex.set-keyword)
                        ((eq? categ 'constant) slatex.set-constant)
                        ((eq? categ 'variable) slatex.set-variable)
                        (else
                         (slatex.error 'slatex.add-to-slatex-db-basic 1))))
          (ids (slatex.read-grouped-schemeids in)))
      (for-each setter ids))))

(define slatex.add-to-slatex-db-special
  (lambda (in what)
    (let ((ids (slatex.read-grouped-schemeids in)))
      (cond ((eq? what 'unsetspecialsymbol)
             (for-each slatex.unset-special-symbol ids))
            ((eq? what 'setspecialsymbol)
             (if (= (length ids) 1)
                 'ok
                 (slatex.error
                  'slatex.add-to-slatex-db-special
                  'setspecialsymbol-takes-one-arg-only))
             (let ((transl (slatex.read-grouped-latexexp in)))
               (slatex.set-special-symbol (car ids) transl)))
            (else (slatex.error 'slatex.add-to-slatex-db-special 2))))))

(define slatex.process-slatex-alias
  (lambda (in what which)
    (let ((triggerer (slatex.read-grouped-latexexp in)))
      (cond ((eq? which 'intext)
             (set! slatex.*intext-triggerers*
                   (what triggerer slatex.*intext-triggerers*)))
            ((eq? which 'resultintext)
             (set! slatex.*resultintext-triggerers*
                   (what triggerer slatex.*resultintext-triggerers*)))
            ((eq? which 'display)
             (set! slatex.*display-triggerers*
                   (what triggerer slatex.*display-triggerers*)))
            ((eq? which 'box)
             (set! slatex.*box-triggerers*
                   (what triggerer slatex.*box-triggerers*)))
            ((eq? which 'input)
             (set! slatex.*input-triggerers*
                   (what triggerer slatex.*input-triggerers*)))
            ((eq? which 'region)
             (set! slatex.*region-triggerers*
                   (what triggerer slatex.*region-triggerers*)))
            ((eq? which 'mathescape)
             (if (= (string-length triggerer) 1)
                 'ok
                 (slatex.error
                  'slatex.process-slatex-alias
                  'math-escape-should-be-character))
             (set! slatex.*math-triggerers*
                   (what (string-ref triggerer 0) slatex.*math-triggerers*)))
            (else (slatex.error 'slatex.process-slatex-alias 2))))))

(define slatex.decide-latex-or-tex
  (lambda (latex?)
    (set! slatex.*latex?* latex?)
    (let ((pltexchk.jnk "pltexchk.jnk"))
      (if (slatex.file-exists? pltexchk.jnk) (slatex.delete-file pltexchk.jnk))
      (if (not slatex.*latex?*)
          (call-with-output-file
              pltexchk.jnk
            (lambda (outp) (display 'junk outp) (newline outp)))))))

(define slatex.process-include-only
  (lambda (in)
    (set! slatex.*include-onlys* '())
    (for-each
     (lambda (filename)
       (let ((filename (slatex.full-texfile-name filename)))
         (if filename
             (set! slatex.*include-onlys*
                   (slatex.adjoin-string filename slatex.*include-onlys*)))))
     (slatex.read-grouped-commaed-filenames in))))

(define slatex.process-documentstyle
  (lambda (in)
    (slatex.eat-latex-whitespace in)
    (if (char=? (peek-char in) #\[)
        (for-each
         (lambda (filename)
           (let ((%:g0% slatex.*slatex-in-protected-region?*))
             (set! slatex.*slatex-in-protected-region?* #f)
             (let ((%temp% (begin
                             (slatex.process-tex-file
                              (string-append filename ".sty")))))
               (set! slatex.*slatex-in-protected-region?* %:g0%)
               %temp%)))
         (slatex.read-bktd-commaed-filenames in)))))

(define slatex.process-case-info
  (lambda (in)
    (let ((bool (slatex.read-grouped-latexexp in)))
      (set! slatex.*slatex-case-sensitive?*
            (cond ((string-ci=? bool "true") #t)
                  ((string-ci=? bool "false") #f)
                  (else
                   (slatex.error
                    'slatex.process-case-info
                    'bad-schemecasesensitive-arg)))))))

(define slatex.seen-first-command? #f)

(define slatex.process-main-tex-file
  (lambda (filename)
                                        ;    (display "SLaTeX v. 2.2")
                                        ;    (newline)
    (set! slatex.*texinputs-list* (slatex.path->list slatex.*texinputs*))
    (let ((file-hide-file "xZfilhid.tex"))
      (if (slatex.file-exists? file-hide-file)
          (slatex.delete-file file-hide-file))
      (if (eq? *op-sys* 'dos)
          (call-with-output-file
              file-hide-file
            (lambda (out) (display "\\def\\filehider{x}" out) (newline out)))))
                                        ;    (display "typesetting code")
    (set! slatex.subjobname (slatex.basename filename ".tex"))
    (set! slatex.seen-first-command? #f)
    (slatex.process-tex-file filename)
                                        ;    (display 'done)
                                        ;    (newline)
    ))

(define slatex.dump-intext
  (lambda (in out)
    (let* ((display (if out display slatex.ignore2))
           (delim-char (begin (slatex.eat-whitespace in) (read-char in)))
           (delim-char (cond ((char=? delim-char #\{) #\}) (else delim-char))))
      (if (eof-object? delim-char) (slatex.error 'slatex.dump-intext 1))
      (let loop ()
        (let ((c (read-char in)))
          (if (eof-object? c) (slatex.error 'slatex.dump-intext 2))
          (if (char=? c delim-char) 'done (begin (display c out) (loop))))))))

(define slatex.dump-display
  (lambda (in out ender)
    (slatex.eat-tabspace in)
    (let ((display (if out display slatex.ignore2))
          (ender-lh (string-length ender))
          (c (peek-char in)))
      (if (eof-object? c) (slatex.error 'slatex.dump-display 1))
      (if (char=? c #\newline) (read-char in))
      (let loop ((buf ""))
        (let ((c (read-char in)))
          (if (eof-object? c) (slatex.error 'slatex.dump-display 2))
          (let ((buf (string-append buf (string c))))
            (if (slatex.string-prefix? buf ender)
                (if (= (string-length buf) ender-lh) 'done (loop buf))
                (begin (display buf out) (loop "")))))))))

(define slatex.debug? #f)

(define slatex.process-tex-file
  (lambda (raw-filename)
    (if slatex.debug?
        (begin (display "begin ") (display raw-filename) (newline)))
    (let ((filename (slatex.full-texfile-name raw-filename)))
      (if (not filename)
          (begin
            (display "[")
            (display raw-filename)
            (display "]")
            (slatex.force-output))
          (call-with-input-file
              filename
            (lambda (in)
              (let ((done? #f))
                (let loop ()
                  (if done?
                      'exit-loop
                      (begin
                        (let ((c (read-char in)))
                          (cond ((eof-object? c) (set! done? #t))
                                ((char=? c #\%) (slatex.eat-till-newline in))
                                ((char=? c #\\)
                                 (let ((cs (slatex.read-ctrl-seq in)))
                                   (if slatex.seen-first-command?
                                       'skip
                                       (begin
                                         (set! slatex.seen-first-command? #t)
                                         (slatex.decide-latex-or-tex
                                          (string=? cs "documentstyle"))))
                                   (cond ((not slatex.*slatex-enabled?*)
                                          (if (string=?
                                               cs
                                               slatex.*slatex-reenabler*)
                                              (slatex.enable-slatex-again)))
                                         ((string=? cs "slatexignorecurrentfile")
                                          (set! done? #t))
                                         ((string=? cs "slatexseparateincludes")
                                          (if slatex.*latex?*
                                              (set! slatex.*slatex-separate-includes?*
                                                    #t)))
                                         ((string=? cs "slatexdisable")
                                          (slatex.disable-slatex-temply in))
                                         ((string=? cs "begin")
                                          (let ((cs (slatex.read-grouped-latexexp
                                                     in)))
                                            (cond ((member cs
                                                           slatex.*display-triggerers*)
                                                   (slatex.trigger-scheme2tex
                                                    'envdisplay
                                                    in
                                                    cs))
                                                  ((member cs
                                                           slatex.*box-triggerers*)
                                                   (slatex.trigger-scheme2tex
                                                    'envbox
                                                    in
                                                    cs))
                                                  ((member cs
                                                           slatex.*region-triggerers*)
                                                   (slatex.trigger-region
                                                    'envregion
                                                    in
                                                    cs)))))
                                         ((member cs slatex.*intext-triggerers*)
                                          (slatex.trigger-scheme2tex
                                           'intext
                                           in
                                           #f))
                                         ((member cs
                                                  slatex.*resultintext-triggerers*)
                                          (slatex.trigger-scheme2tex
                                           'resultintext
                                           in
                                           #f))
                                         ((member cs slatex.*display-triggerers*)
                                          (slatex.trigger-scheme2tex
                                           'plaindisplay
                                           in
                                           cs))
                                         ((member cs slatex.*box-triggerers*)
                                          (slatex.trigger-scheme2tex
                                           'plainbox
                                           in
                                           cs))
                                         ((member cs slatex.*region-triggerers*)
                                          (slatex.trigger-region
                                           'plainregion
                                           in
                                           cs))
                                         ((member cs slatex.*input-triggerers*)
                                          (slatex.process-scheme-file
                                           (slatex.read-filename in)))
                                         ((string=? cs "input")
                                          (let ((%:g1% slatex.*slatex-in-protected-region?*))
                                            (set! slatex.*slatex-in-protected-region?*
                                                  #f)
                                            (let ((%temp% (begin
                                                            (slatex.process-tex-file
                                                             (slatex.read-filename
                                                              in)))))
                                              (set! slatex.*slatex-in-protected-region?*
                                                    %:g1%)
                                              %temp%)))
                                         ((string=? cs "include")
                                          (if slatex.*latex?*
                                              (let ((f (slatex.full-texfile-name
                                                        (slatex.read-filename in))))
                                                (if (and f
                                                         (or (eq? slatex.*include-onlys*
                                                                  'all)
                                                             (member f
                                                                     slatex.*include-onlys*)))
                                                    (let ((%:g2% slatex.*slatex-in-protected-region?*)
                                                          (%:g3% slatex.subjobname)
                                                          (%:g4% primary-aux-file-count))
                                                      (set! slatex.*slatex-in-protected-region?*
                                                            #f)
                                                      (set! slatex.subjobname
                                                            slatex.subjobname)
                                                      (set! primary-aux-file-count
                                                            primary-aux-file-count)
                                                      (let ((%temp% (begin
                                                                      (if slatex.*slatex-separate-includes?*
                                                                          (begin
                                                                            (set! slatex.subjobname
                                                                                  (slatex.basename
                                                                                   f
                                                                                   ".tex"))
                                                                            (set! primary-aux-file-count
                                                                                  -1)))
                                                                      (slatex.process-tex-file
                                                                       f))))
                                                        (set! slatex.*slatex-in-protected-region?*
                                                              %:g2%)
                                                        (set! slatex.subjobname %:g3%)
                                                        (set! primary-aux-file-count
                                                              %:g4%)
                                                        %temp%))))))
                                         ((string=? cs "includeonly")
                                          (if slatex.*latex?*
                                              (slatex.process-include-only in)))
                                         ((string=? cs "documentstyle")
                                          (if slatex.*latex?*
                                              (slatex.process-documentstyle in)))
                                         ((string=? cs "schemecasesensitive")
                                          (slatex.process-case-info in))
                                         ((string=? cs "defschemetoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'intext))
                                         ((string=? cs "undefschemetoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'intext))
                                         ((string=? cs "defschemeresulttoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'resultintext))
                                         ((string=? cs "undefschemeresulttoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'resultintext))
                                         ((string=? cs "defschemedisplaytoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'display))
                                         ((string=? cs "undefschemedisplaytoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'display))
                                         ((string=? cs "defschemeboxtoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'box))
                                         ((string=? cs "undefschemeboxtoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'box))
                                         ((string=? cs "defschemeinputtoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'input))
                                         ((string=? cs "undefschemeinputtoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'input))
                                         ((string=? cs "defschemeregiontoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-string
                                           'region))
                                         ((string=? cs "undefschemeregiontoken")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-string!
                                           'region))
                                         ((string=? cs "defschememathescape")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.adjoin-char
                                           'mathescape))
                                         ((string=? cs "undefschememathescape")
                                          (slatex.process-slatex-alias
                                           in
                                           slatex.remove-char!
                                           'mathescape))
                                         ((string=? cs "setkeyword")
                                          (slatex.add-to-slatex-db in 'keyword))
                                         ((string=? cs "setconstant")
                                          (slatex.add-to-slatex-db in 'constant))
                                         ((string=? cs "setvariable")
                                          (slatex.add-to-slatex-db in 'variable))
                                         ((string=? cs "setspecialsymbol")
                                          (slatex.add-to-slatex-db
                                           in
                                           'setspecialsymbol))
                                         ((string=? cs "unsetspecialsymbol")
                                          (slatex.add-to-slatex-db
                                           in
                                           'unsetspecialsymbol)))))))
                        (loop)))))))))
    (if slatex.debug?
        (begin (display "end ") (display raw-filename) (newline)))))

(define slatex.process-scheme-file
  (lambda (raw-filename)
    (let ((filename (slatex.full-scmfile-name raw-filename)))
      (if (not filename)
          (begin
            (display "process-scheme-file: ")
            (display raw-filename)
            (display " doesn't exist")
            (newline))
          (let ((aux.tex (slatex.new-aux-file ".tex")))
                                        ;          (display ".")
            (slatex.force-output)
            (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
            (call-with-input-file
                filename
              (lambda (in)
                (call-with-output-file
                    aux.tex
                  (lambda (out)
                    (let ((%:g5% slatex.*intext?*)
                          (%:g6% slatex.*code-env-spec*))
                      (set! slatex.*intext?* #f)
                      (set! slatex.*code-env-spec* "ZZZZschemedisplay")
                      (let ((%temp% (begin (scheme2tex in out))))
                        (set! slatex.*intext?* %:g5%)
                        (set! slatex.*code-env-spec* %:g6%)
                        %temp%))))))
            (if slatex.*slatex-in-protected-region?*
                (set! slatex.*protected-files*
                      (cons aux.tex slatex.*protected-files*)))
            (slatex.process-tex-file filename))))))

(define slatex.trigger-scheme2tex
  (lambda (typ in env)
    (let* ((aux (slatex.new-aux-file))
           (aux.scm (string-append aux ".scm"))
           (aux.tex (string-append aux ".tex")))
      (if (slatex.file-exists? aux.scm) (slatex.delete-file aux.scm))
      (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
                                        ;      (display ".")
      (slatex.force-output)
      (call-with-output-file
          aux.scm
        (lambda (out)
          (cond ((memq typ '(intext resultintext)) (slatex.dump-intext in out))
                ((memq typ '(envdisplay envbox))
                 (slatex.dump-display in out (string-append "\\end{" env "}")))
                ((memq typ '(plaindisplay plainbox))
                 (slatex.dump-display in out (string-append "\\end" env)))
                (else (slatex.error 'slatex.trigger-scheme2tex 1)))))
      (call-with-input-file
          aux.scm
        (lambda (in)
          (call-with-output-file
              aux.tex
            (lambda (out)
              (let ((%:g7% slatex.*intext?*) (%:g8% slatex.*code-env-spec*))
                (set! slatex.*intext?* (memq typ '(intext resultintext)))
                (set! slatex.*code-env-spec*
                      (cond ((eq? typ 'intext) "ZZZZschemecodeintext")
                            ((eq? typ 'resultintext) "ZZZZschemeresultintext")
                            ((memq typ '(envdisplay plaindisplay))
                             "ZZZZschemedisplay")
                            ((memq typ '(envbox plainbox)) "ZZZZschemebox")
                            (else (slatex.error 'slatex.trigger-scheme2tex 2))))
                (let ((%temp% (begin (scheme2tex in out))))
                  (set! slatex.*intext?* %:g7%)
                  (set! slatex.*code-env-spec* %:g8%)
                  %temp%))))))
      (if slatex.*slatex-in-protected-region?*
          (set! slatex.*protected-files*
                (cons aux.tex slatex.*protected-files*)))
      (if (memq typ '(envdisplay plaindisplay envbox plainbox))
          (slatex.process-tex-file aux.tex))
      (slatex.delete-file aux.scm))))

(define slatex.trigger-region
  (lambda (typ in env)
    (let ((aux.tex (slatex.new-primary-aux-file ".tex"))
          (aux2.tex (slatex.new-secondary-aux-file ".tex")))
      (if (slatex.file-exists? aux2.tex) (slatex.delete-file aux2.tex))
      (if (slatex.file-exists? aux.tex) (slatex.delete-file aux.tex))
                                        ;      (display ".")
      (slatex.force-output)
      (let ((%:g9% slatex.*slatex-in-protected-region?*)
            (%:g10% slatex.*protected-files*))
        (set! slatex.*slatex-in-protected-region?* #t)
        (set! slatex.*protected-files* '())
        (let ((%temp% (begin
                        (call-with-output-file
                            aux2.tex
                          (lambda (out)
                            (cond ((eq? typ 'envregion)
                                   (slatex.dump-display
                                    in
                                    out
                                    (string-append "\\end{" env "}")))
                                  ((eq? typ 'plainregion)
                                   (slatex.dump-display
                                    in
                                    out
                                    (string-append "\\end" env)))
                                  (else
                                   (slatex.error 'slatex.trigger-region 1)))))
                        (slatex.process-tex-file aux2.tex)
                        (set! slatex.*protected-files*
                              (slatex.reverse! slatex.*protected-files*))
                        (call-with-input-file
                            aux2.tex
                          (lambda (in)
                            (call-with-output-file
                                aux.tex
                              (lambda (out)
                                (slatex.inline-protected-files in out)))))
                        (slatex.delete-file aux2.tex))))
          (set! slatex.*slatex-in-protected-region?* %:g9%)
          (set! slatex.*protected-files* %:g10%)
          %temp%)))))

(define slatex.inline-protected-files
  (lambda (in out)
    (let ((done? #f))
      (let loop ()
        (if done?
            'exit-loop
            (begin
              (let ((c (read-char in)))
                (cond ((eof-object? c) (display "{}" out) (set! done? #t))
                      ((char=? c #\%) (slatex.eat-till-newline in))
                      ((char=? c #\\)
                       (let ((cs (slatex.read-ctrl-seq in)))
                         (cond ((string=? cs "begin")
                                (let ((cs (slatex.read-grouped-latexexp in)))
                                  (cond ((member cs slatex.*display-triggerers*)
                                         (slatex.inline-protected
                                          'envdisplay
                                          in
                                          out
                                          cs))
                                        ((member cs slatex.*box-triggerers*)
                                         (slatex.inline-protected
                                          'envbox
                                          in
                                          out
                                          cs))
                                        ((member cs slatex.*region-triggerers*)
                                         (slatex.inline-protected
                                          'envregion
                                          in
                                          out
                                          cs))
                                        (else
                                         (display "\\begin{" out)
                                         (display cs out)
                                         (display "}" out)))))
                               ((member cs slatex.*intext-triggerers*)
                                (slatex.inline-protected 'intext in out #f))
                               ((member cs slatex.*resultintext-triggerers*)
                                (slatex.inline-protected
                                 'resultintext
                                 in
                                 out
                                 #f))
                               ((member cs slatex.*display-triggerers*)
                                (slatex.inline-protected
                                 'plaindisplay
                                 in
                                 out
                                 cs))
                               ((member cs slatex.*box-triggerers*)
                                (slatex.inline-protected 'plainbox in out cs))
                               ((member cs slatex.*region-triggerers*)
                                (slatex.inline-protected 'plainregion in out cs))
                               ((member cs slatex.*input-triggerers*)
                                (slatex.inline-protected 'input in out cs))
                               (else (display "\\" out) (display cs out)))))
                      (else (display c out))))
              (loop)))))))

(define slatex.inline-protected
  (lambda (typ in out env)
    (cond ((eq? typ 'envregion)
           (display "\\begin{" out)
           (display env out)
           (display "}" out)
           (slatex.dump-display in out (string-append "\\end{" env "}"))
           (display "\\end{" out)
           (display env out)
           (display "}" out))
          ((eq? typ 'plainregion)
           (display "\\" out)
           (display env out)
           (slatex.dump-display in out (string-append "\\end" env))
           (display "\\end" out)
           (display env out))
          (else
           (let ((f (car slatex.*protected-files*)))
             (set! slatex.*protected-files* (cdr slatex.*protected-files*))
             (call-with-input-file
                 f
               (lambda (in) (slatex.inline-protected-files in out)))
             (slatex.delete-file f))
           (cond ((memq typ '(intext resultintext)) (slatex.dump-intext in #f))
                 ((memq typ '(envdisplay envbox))
                  (slatex.dump-display in #f (string-append "\\end{" env "}")))
                 ((memq typ '(plaindisplay plainbox))
                  (slatex.dump-display in #f (string-append "\\end" env)))
                 ((eq? typ 'input) (slatex.read-filename in))
                 (else (slatex.error 'slatex.inline-protected 1)))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 input2)
         (s1 input1)
         (name "slatex"))
    (run-r7rs-benchmark
     (string-append name ":" s3)
     count
     (lambda () (slatex.process-main-tex-file (hide count input1)))
     (lambda (result) #t))))

(with-input-from-file (bench-input "slatex") run-benchmark)
