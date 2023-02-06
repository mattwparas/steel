(require-builtin steel/time)
(require-builtin steel/strings/colors as colors.)

(provide log/info! log/warn! log/debug!)

(define *info* (colors.green "INFO"))
(define *warn* (colors.red "WARN"))
(define *debug* (colors.blue "DEBUG"))

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


