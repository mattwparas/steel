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

(define (log! level arg-list)
    (apply displayln (append
                        (list 
                            (local-time/now! "%Y-%m-%dT%H:%M:%S")
                            " ["
                            level
                            "] - ")
                        arg-list)))

(define (log/info! . args)
    (log! *info* args))

(define (log/warn! . args)
    (log! *warn* args))

(define (log/debug! . args)
    (log! *debug* args))

(define (log/error! . args)
    (log! *error* args))

(define (log/trace! . args)
    (log! *trace* args))