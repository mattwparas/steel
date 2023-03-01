(provide find-function-pointer)

(define *modules* 'uninitialized)

(define (load-modules)
    (when (symbol? *modules*)
          (set! *modules* (%list-modules!))))

(define (get-modules!) 
    (load-modules)
    *modules*)


(define (find-function-pointer function-ptr)
    (define found
        (transduce (get-modules!) 
                   (mapping (lambda (mod) (%module/lookup-function mod function-ptr))) 
                   (filtering string?) 
                   (into-list)))

    (if found (car found) #f))

