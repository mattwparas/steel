(provide header-name=?
         token?
         normalize-headers
         header-ref
         header-ref*
         header-contains?
         header-set
         header-add
         header-default
         header-remove
         header-names
         headers->string
         parse-header-line)

;;@doc
;; Whether two header names are the same name. Header names are not case
;; sensitive.
(define (header-name=? left right)
  (string-ci=? left right))

(define (header-value->string value)
  (cond
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [(number? value) (number->string value)]
    [else (error "http: a header value has to be a string, symbol or number, got:" value)]))

(define (token-char? char)
  (define code (char->integer char))
  (or (and (>= code 48) (<= code 57))
      (and (>= code 65) (<= code 90))
      (and (>= code 97) (<= code 122))
      (string-contains? "!#$%&'*+-.^_`|~" (string char))))

;;@doc
;; Whether *str* is a token: letters, digits and `!#$%&'*+-.^_`|~`, and at
;; least one of them. A header name is one, and so is a request method.
;; Anything else, a space included, is read differently by different proxies.
(define (token? str)
  (and (> (string-length str) 0)
       (let loop ([at 0])
         (or (>= at (string-length str)) (and (token-char? (string-ref str at)) (loop (+ at 1)))))))

;; A value carrying a newline would let a caller write their own headers after
;; it, so none of them get through.
(define (check-header name value)
  (unless (token? name)
    (error "http: not a usable header name:" name))
  (when (or (string-contains? value "\r") (string-contains? value "\n"))
    (error "http: a header value cannot contain a newline:" value))
  (cons name value))

;;@doc
;; Accepts headers given as an association list, as a list of two element
;; lists, or as a hashmap, and returns the association list the rest of these
;; functions take.
(define (normalize-headers headers)
  (cond
    [(hash? headers)
     (normalize-headers (map (lambda (key) (cons key (hash-get headers key)))
                             (hash-keys->list headers)))]
    [(list? headers)
     (map (lambda (entry)
            (cond
              [(and (list? entry) (equal? (length entry) 2))
               (check-header (header-value->string (car entry))
                             (header-value->string (list-ref entry 1)))]
              [(pair? entry)
               (check-header (header-value->string (car entry)) (header-value->string (cdr entry)))]
              [else (error "http: not a usable header:" entry)]))
          headers)]
    [else (error "http: headers have to be a list or a hashmap, got:" headers)]))

;;@doc
;; The first value held under *name*, or *default* when there is none.
(define (header-ref headers name [default #f])
  (let loop ([rest headers])
    (cond
      [(empty? rest) default]
      [(header-name=? (car (car rest)) name) (cdr (car rest))]
      [else (loop (cdr rest))])))

;;@doc
;; Every value held under *name*, in order.
(define (header-ref* headers name)
  (map cdr (filter (lambda (entry) (header-name=? (car entry) name)) headers)))

;;@doc
;; Whether *name* is present at all.
(define (header-contains? headers name)
  (not (empty? (header-ref* headers name))))

;;@doc
;; Drops every value held under *name*.
(define (header-remove headers name)
  (filter (lambda (entry) (not (header-name=? (car entry) name))) headers))

;;@doc
;; Adds *name* at the end, keeping any value already held under it. This is
;; what repeated headers want.
(define (header-add headers name value)
  (append headers (list (check-header name (header-value->string value)))))

;;@doc
;; Puts *name* at the end with just this value, dropping anything already held
;; under it.
(define (header-set headers name value)
  (header-add (header-remove headers name) name value))

;;@doc
;; Sets *name* only if it is not already there, which is how the client fills
;; in a header without taking one away from the caller.
(define (header-default headers name value)
  (if (header-contains? headers name)
      headers
      (header-add headers name value)))

;;@doc
;; The names, in order, as they were given.
(define (header-names headers)
  (map car headers))

;;@doc
;; Writes headers out as the lines of a request or response head, each one
;; ending in a crlf. The blank line that ends the head is not included.
(define (headers->string headers)
  (apply string-append
         (map (lambda (entry) (string-append (car entry) ": " (cdr entry) "\r\n")) headers)))

;;@doc
;; Reads one header line into a (name . value) pair, or answers `#false` when
;; the line is not a header. Surrounding whitespace is dropped from the value,
;; and kept inside it.
(define (parse-header-line line)
  (define split (split-once line ":"))
  (if (or (not (list? split)) (equal? (car split) ""))
      #f
      (cons (car split) (trim (list-ref split 1)))))
