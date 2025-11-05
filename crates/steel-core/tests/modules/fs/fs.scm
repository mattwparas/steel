(provide walk-files
         file->string)

(define (file->string path)
  (let ([file (open-input-file path)]) (read-port-to-string file)))

;; Walk the file system, applying a function to each file found
(define/contract (walk-files path func)
  (->/c string? (->/c string? any/c) any/c)
  (cond
    [(is-file? path) (func path)]
    [(is-dir? path) (for-each (lambda (x) (walk-files x func)) (read-dir path))]
    [else void]))
