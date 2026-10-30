(export define-type define-type-body define-type-help)

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
     (or ((#%join-identifiers variant ?) x)
         (define-type-body x rest ...))]
    [(define-type-body x (variant fields))
     ((#%join-identifiers variant ?) x)]))


(define-syntax define-type
  (syntax-rules ()
    [(define-type name (variant fields) rest ...)
     (begin
       (define ((#%join-identifiers name ?) x)
         (define-type-body x (variant fields) rest ...))
       (define-type-help name (variant fields) rest ...))]
    [(define-type-test name (variant fields))
     (begin
       (define ((#%join-identifiers name ?) x)
         (define-type-body x (variant fields)))
       (define-type-help name (variant fields)))]))