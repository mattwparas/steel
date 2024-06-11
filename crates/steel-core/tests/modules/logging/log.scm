(require-builtin steel/time)

(provide log!
         log/info!
         log/warn!
         log/debug!
         log/error!)

(define *info* "INFO")
(define *warn* "WARN")
(define *debug* "DEBUG")
(define *error* "ERROR")
(define *trace* "TRACE")

;;@doc
;; Log directly on the specified level the with arguments, as a list
(define (log! level arg-list)
  (apply displayln (append (list (local-time/now! "%Y-%m-%dT%H:%M:%S") " [" level "] - ") arg-list)))

;;@doc
;; Log the arguments using the *info* target, i.e. log on INFO
(define (log/info! . args)
  (log! *info* args))

;;@doc
;; Log the arguments using the *warn* target, i.e. log on WARN
(define (log/warn! . args)
  (log! *warn* args))

;;@doc
;; Log the arguments using the *debug* target, i.e. log on DEBUG
(define (log/debug! . args)
  (log! *debug* args))

;;@doc
;; Log the arguments using the *error* target, i.e. log on ERROR
(define (log/error! . args)
  (log! *error* args))

;;@doc
;; Log the arguments using the *trace* target, i.e. log on TRACE
(define (log/trace! . args)
  (log! *trace* args))
