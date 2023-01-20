(require "steel/contracts/contract.scm" (for-syntax "steel/contracts/contract.scm"))

(provide walk-files)


; (define (tree p)
;   (define (tree-rec path padding)
;     (define name (file-name path))
;     (displayln (string-append padding name))
;     (cond [(is-file? path) name]
;           [(is-dir? path)
;             (map (fn (x)
;                     (tree-rec x (string-append padding "    ")))
;                   ; (merge-sort (read-dir path)))]
;                   (read-dir path))]
;           [else void]))
;   (flatten (tree-rec p "")))


(define (for-each func lst)
    (if (null? lst) 
        void
        (begin
            (func (car lst))
            (when (null? lst) (return! void))
            (for-each func (cdr lst)))))

;; Walk the file system, applying a function to each file found
(define/c (walk-files path func)
    (->c string? (->c string? any/c) any/c)
    (cond [(is-file? path) (func path)]
          [(is-dir? path) (for-each (lambda (x) (walk-files x func)) (read-dir path))]
          [else void]))


