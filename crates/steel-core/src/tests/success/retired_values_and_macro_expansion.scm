;; A thread takes a value out of a cell other threads are reading, and then
;; expands a macro, which runs another engine on the same thread. That
;; engine's own cells have no readers, which says nothing about the first
;; value.

;; Defined through eval so that it is there when this file runs as a module.
(eval '(define-syntax (churn-in-the-expander stx)
         (syntax-case stx ()
           [(_)
            (begin
              (let ([b (box '())])
                (let loop ([i 0])
                  (when (< i 400)
                    (set-box! b (list i i))
                    (loop (+ i 1)))))
              #'1)])))

(define shared (box (list 0 0)))
(define stop (box #f))
(define wrong (box 0))

(define (reader)
  (let loop ([n 0])
    (let ([v (unbox shared)])
      (unless (and (pair? v) (equal? (car v) (cadr v)))
        (set-box! wrong (+ 1 (unbox wrong)))))
    (if (and (= 0 (modulo n 1024)) (unbox stop))
        n
        (loop (+ n 1)))))

(define readers (map (lambda (_) (spawn-native-thread reader)) (range 0 96)))

(define (writer i)
  (when (> i 0)
    (set-box! shared (list i i))
    (eval '(churn-in-the-expander))
    (writer (- i 1))))

(writer 3000)
(set-box! stop #t)
(for-each thread-join! readers)
(assert! (= (unbox wrong) 0))
