(define-syntax case
  (syntax-rules (else =>)
    [(case arg [else => e1 ...])
     (begin e1 ...)]
    [(case arg [e1 => e2 ...])
     (when (e1 arg) e2 ...)]
    [(case arg [e1 => e2 ...] c1 ...)
     (if (e1 arg)
         (begin e2 ...)
         (case arg c1 ...))]))

;; (define-syntax cond
;;   (syntax-rules (else)
;;     [(cond [else e1 ...])
;;      (begin e1 ...)]
;;     [(cond [e1 e2 ...])
;;      (when e1 e2 ...)]
;;     [(cond [e1 e2 ...] c1 ...)
;;      (if e1
;;          (begin e2 ...)
;;          (cond c1 ...))]))

(define (pattern-match x)
  (case x
    [number? =>
             (displayln "Found a number!")
             (displayln "second statement")]
    [function? =>
               (displayln "found a function")
               (displayln "found some other noise")]
    [else => (displayln "else case!")]))


(case +
    [number? =>
             (displayln "Found a number!")
             (displayln "second statement")]
    [function? =>
               (displayln "found a function")
               (displayln "found some other noise")]
    [else => (displayln "else case!")])

;; (if (number? 15)
;;     (begin (displayln \"Found a number!\") (displayln \"second statement\"))
;;     (if function?
;;         (begin => (displayln \"found a function\")
;;                (displayln \"found some other noise\"))
;;         (begin => (displayln \"else case!\"))))
