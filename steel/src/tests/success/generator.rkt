(define (for-each func lst)
    (if (null? lst) 
        void
        (begin
            (func (car lst))
            (when (null? lst) (return! void))
            (for-each func (cdr lst)))))

; (for-each (lambda (x) (displayln x)) '(1 2 3 4 5))

;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)
;; Both internal functions are closures over lst

    ;; Internal variable/Function which passes the current element in a list
    ;; to its return argument (which is a continuation), or passes an end-of-list marker 
    ;; if no more elements are left. On each step the function name is 
    ;; rebound to a continuation which points back into the function body,
    ;; while return is rebound to whatever continuation the caller specifies.
    (define (control-state return)
        (for-each 
            (lambda (element)
                    (set! return (call/cc
                                    (lambda (resume-here)
                                        ;; Grab the current continuation
                                        (set! control-state resume-here)
                                        (return element))))) ;; (return element) evaluates to next return
            lst)
        (return 'you-fell-off-the-end))

    ;; (-> X u 'you-fell-off-the-end)
    ;; This is the actual generator, producing one item from a-list at a time.
    (define (generator)
        (call/cc control-state))

    ;; Return the generator 
    generator)


    ;; So this seems to get the job done
    ;; looks like defining the local variable and then returning it makes it get
    ;; confused for some reason...
    ; (lambda () (call/cc control-state)))

(define generate-digit
    (generate-one-element-at-a-time '(0 1 2)))

(assert! (equal? 0 (generate-digit))) ;; 0
(assert! (equal? 1 (generate-digit))) ;; 1
(assert! (equal? 2 (generate-digit))) ;; 2
(assert! (equal? 'you-fell-off-the-end (generate-digit))) ;; you-fell-off-the-end


; (let ((control-state 123) (generator 123))
;   (let ((#####control-state0 (λ (return)
;         (begin
;          (for-each
;              (λ (element)
;                (set! return
;                  (call/cc
;                    (λ (resume-here)
;                      (begin
;                       (set! control-state resume-here)
;                        (return element))))))
;              lst)
;           (return (quote you-fell-off-the-end)))))
;       (#####generator1 (λ () (call/cc control-state))))
;     (begin
;       (set! control-state #####control-state0)
;       (set! generator #####generator1)
;       generator)))