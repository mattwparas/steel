(provide header-name=?
         token?
         normalize-headers
         header-ref
         header-ref*
         header-tokens
         header-contains?
         header-set
         header-add
         header-default
         header-remove
         header-names
         headers->string
         parse-header-line)

;;@doc
;; Whether two header names are the same, ignoring case
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
      (string-contains? "!#$%&'*+-.^_`|~" char)))

;;@doc
;; Whether `str` is an http token: letters, digits and `!#$%&'*+-.^_`|~`
;; string-ref walks the string from the start for every index, so this walks the
;; characters once instead. Taking one stops at the first that fails
(define (every-char? str ok?)
  (equal? 0
          (transduce str
                     (compose (filtering (lambda (char) (not (ok? char)))) (taking 1))
                     (into-count))))

(define (token? str)
  (and (not (equal? str "")) (every-char? str token-char?)))

(define (check-header name value)
  (unless (token? name)
    (error "http: not a usable header name:" name))
  (when (or (string-contains? value "\r") (string-contains? value "\n"))
    (error "http: a header value cannot contain a newline:" value))
  (cons name value))

;;@doc
;; Converts an alist, a list of two element lists, or a hashmap into a header
;; alist
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
;; The first value held under `name`, or `default` when there is none
(define (header-ref headers name [default #f])
  (let loop ([rest headers])
    (cond
      [(empty? rest) default]
      [(header-name=? (car (car rest)) name) (cdr (car rest))]
      [else (loop (cdr rest))])))

;;@doc
;; Every value held under `name`, in order
(define (header-ref* headers name)
  (map cdr (filter (lambda (entry) (header-name=? (car entry) name)) headers)))

;;@doc
;; Every comma separated value held under `name`, lowercased and in order. A
;; field written over several lines means the same as one line holding its
;; values in that order.
(define (header-tokens headers name)
  (filter (lambda (token) (not (equal? token "")))
          (map (lambda (part) (string-downcase (trim part)))
               (apply append
                      (map (lambda (value) (split-many value ","))
                           (header-ref* headers name))))))

;;@doc
;; Whether `name` is present
(define (header-contains? headers name)
  (not (empty? (header-ref* headers name))))

;;@doc
;; Removes every value held under `name`.
(define (header-remove headers name)
  (filter (lambda (entry) (not (header-name=? (car entry) name))) headers))

;;@doc
;; Adds `name` at the end, keeping any value already there.
(define (header-add headers name value)
  (append headers (list (check-header name (header-value->string value)))))

;;@doc
;; Sets `name` at the end, removing any value already there.
(define (header-set headers name value)
  (header-add (header-remove headers name) name value))

;;@doc
;; Sets `name` only when it is not already present.
(define (header-default headers name value)
  (if (header-contains? headers name)
      headers
      (header-add headers name value)))

;;@doc
;; The header names, in order
(define (header-names headers)
  (map car headers))

;;@doc
;; Formats headers as crlf terminated head lines, without the blank line that
;; ends the head.
(define (headers->string headers)
  (apply string-append
         (map (lambda (entry) (string-append (car entry) ": " (cdr entry) "\r\n")) headers)))

;;@doc
;; Parses one header line into a trimmed `(name . value)` pair, or `#false`.
(define (parse-header-line line)
  (define split (split-once line ":"))
  (if (or (not (list? split)) (equal? (car split) ""))
      #f
      (cons (car split) (trim (list-ref split 1)))))
