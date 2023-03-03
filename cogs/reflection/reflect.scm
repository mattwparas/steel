(provide find-function-pointer reset-modules!)

(define *modules* 'uninitialized)

(define (reset-modules!)
    (set! *modules* 'uninitialized))

(define (load-modules)
    (when (symbol? *modules*)
          (set! *modules* (%list-modules!))))

(define (get-modules!) 
    (load-modules)
    *modules*)

;;@doc
;; Attempts to search for the name of the function from the global module table
;; If not found, returns #false.
;; 
;; This lazily loads the modules - if for whatever reason, a module is loaded _after_ this has been
;; called, the module table in memory will need to be refreshed using `reset-modules!` 
(define (find-function-pointer function-ptr)
    (define found
        (transduce (get-modules!) 
                   (mapping (lambda (mod) (%module/lookup-function mod function-ptr))) 
                   (filtering (lambda (x) (not (boolean? x))))
                   (into-list)))

    ; found)

    (if found (car found) #f))

