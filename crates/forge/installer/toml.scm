(provide toml-ref)

(struct ScannedLine (text open depth-change))
(struct StringSpan (chars rest))

(define (toml-ref contents table key)
  (define target (string-append table "." key))
  (define (search lines section open depth)
    (cond
      [(empty? lines) #f]
      [else
       (define scanned (scan-line (car lines) open))
       (define text (trim (ScannedLine-text scanned)))
       (define next-open (ScannedLine-open scanned))
       (define next-depth (+ depth (ScannedLine-depth-change scanned)))

       (cond
         [(or open (> depth 0)) (search (cdr lines) section next-open next-depth)]

         [(starts-with? text "[") (search (cdr lines) (header-name text) next-open next-depth)]

         [else
          (define value (matching-value text section target))
          (if value
              value
              (search (cdr lines) section next-open next-depth))])]))

  (search (split-many contents "\n") #f #f 0))

(define (header-name text)
  (if (starts-with? text "[[")
      #f
      (trim (trim-end-matches (trim-start-matches text "[") "]"))))

(define (matching-value text section target)
  (define key-and-value (split-at-equals text))
  (and key-and-value
       (equal? (qualify section (car key-and-value)) target)
       (read-string-value (cdr key-and-value))))

(define (qualify section key)
  (if section
      (string-append section "." key)
      key))

(define (scan-line line open)

  (define (scan chars open acc depth-change)
    (cond
      [(empty? chars) (ScannedLine (list->string (reverse acc)) open depth-change)]

      [open
       (if (triple-quote? chars open)
           (scan (cdddr chars) #f (push-quotes open acc) depth-change)
           (scan (cdr chars) open (cons (car chars) acc) depth-change))]

      [(eqv? (car chars) #\#) (ScannedLine (list->string (reverse acc)) #f depth-change)]

      [(or (eqv? (car chars) #\") (eqv? (car chars) #\'))
       (define quote-char (car chars))
       (if (triple-quote? chars quote-char)
           (scan (cdddr chars) quote-char (push-quotes quote-char acc) depth-change)
           (let ([span (take-string (cdr chars) quote-char)])
             (scan (StringSpan-rest span)
                   #f
                   (append (StringSpan-chars span) (cons quote-char acc))
                   depth-change)))]

      [(or (eqv? (car chars) #\[) (eqv? (car chars) #\{))
       (scan (cdr chars) #f (cons (car chars) acc) (+ depth-change 1))]

      [(or (eqv? (car chars) #\]) (eqv? (car chars) #\}))
       (scan (cdr chars) #f (cons (car chars) acc) (- depth-change 1))]

      [else (scan (cdr chars) #f (cons (car chars) acc) depth-change)]))

  (scan (string->list line) open '() 0))

(define (triple-quote? chars quote-char)
  (and (eqv? (car chars) quote-char)
       (not (empty? (cdr chars)))
       (eqv? (cadr chars) quote-char)
       (not (empty? (cddr chars)))
       (eqv? (caddr chars) quote-char)))

(define (push-quotes quote-char acc)
  (cons quote-char (cons quote-char (cons quote-char acc))))

(define (take-string chars quote-char)

  (define (take chars acc)
    (cond
      [(empty? chars) (StringSpan acc chars)]

      ;; Literal strings ('...') have no escapes
      [(and (eqv? quote-char #\") (eqv? (car chars) #\\) (not (empty? (cdr chars))))
       (take (cddr chars) (cons (cadr chars) (cons #\\ acc)))]

      [(eqv? (car chars) quote-char) (StringSpan (cons quote-char acc) (cdr chars))]

      [else (take (cdr chars) (cons (car chars) acc))]))

  (take chars '()))

(define (split-at-equals text)
  (define (split chars key)
    (cond
      [(empty? chars) #f]
      [(eqv? (car chars) #\=) (cons (trim (list->string (reverse key))) (list->string (cdr chars)))]
      [else (split (cdr chars) (cons (car chars) key))]))

  (split (string->list text) '()))

(define (read-string-value text)
  (define chars (string->list (trim text)))
  (cond
    [(empty? chars) #f]
    [(not (or (eqv? (car chars) #\") (eqv? (car chars) #\'))) #f]

    [(triple-quote? chars (car chars)) #f]

    [else
     (define quote-char (car chars))
     (define taken (StringSpan-chars (take-string (cdr chars) quote-char)))
     (and (not (empty? taken))
          (eqv? (car taken) quote-char)
          (unescape (list->string (reverse (cdr taken))) quote-char))]))

(define (unescape text quote-char)
  (define (walk chars acc)
    (cond
      [(empty? chars) (list->string (reverse acc))]
      [(and (eqv? (car chars) #\\) (not (empty? (cdr chars))))
       (walk (cddr chars) (cons (escape-char (cadr chars)) acc))]
      [else (walk (cdr chars) (cons (car chars) acc))]))

  (if (eqv? quote-char #\')
      text
      (walk (string->list text) '())))

(define (escape-char char)
  (cond
    [(eqv? char #\n) #\newline]
    [(eqv? char #\t) #\tab]
    [(eqv? char #\r) #\return]
    [else char]))
