(require-builtin steel/time)
(require-builtin steel/strings/colors as colors.)

(provide log! log/info! log/warn! log/debug! log/error!)

(define *info* (colors.green "INFO"))
(define *warn* (colors.yellow "WARN"))
(define *debug* (colors.blue "DEBUG"))
(define *error* (colors.red "ERROR"))
(define *trace* (colors.purple "TRACE"))

; (displayln "LOADING MODULE")

; (define (log! level target arg-list)
;     (apply displayln (append
;                     (list 
;                         (local-time/now! "%Y-%m-%dT%H:%M:%S")
;                         " ["
;                         level
;                         " "
;                         target
;                         "] - ")
;                     arg-list)))

;;@doc
;; Log directly on the specified level the with arguments, as a list
(define (log! level arg-list)
    (apply displayln (append
                        (list 
                            (local-time/now! "%Y-%m-%dT%H:%M:%S")
                            " ["
                            level
                            "] - ")
                        arg-list)))

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