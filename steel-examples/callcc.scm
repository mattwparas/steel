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
                              ;  (displayln "GETTING HERE")
                               (return element))))) ;; (return element) evaluates to next return
     lst)
    ;; TODO
    ;; this is reading the local variable
    ;; but it needs to be reading from the heap
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time.
  ; (define (generator)
  ;   (call/cc control-state))

  (lambda () (call/cc control-state)))

;; TODO: don't use IDs for ordering - find something (depth + stack offset or arg position) that will better identify the position of a variable. The fact is that certain variables are created later, which means the offsets are all goofy.
; (define generate-one-element-at-a-time
;   (λ (lst)
;     ((λ (control-state)
;          ((λ (control-state0)
;               (begin
;                (set! control-state control-state0)
;                 (λ ()
;                   (call/cc control-state))))
;             (λ (return)
;               (begin
;                (for-each
;                    (λ (element)
;                      (set! return
;                        (call/cc
;                           (λ (resume-here)
;                             (begin
;                              (set! control-state
;                                 resume-here)
;                               (return element))))))
;                    lst)
;                 (return (quote you-fell-off-the-end))))))
;        123)))

  ;; Return the generator 
  ; generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

; (generate-digit) ;; 0
; (generate-digit) ;; 1
; (generate-digit) ;; 2
; (generate-digit) ;; you-fell-off-the-end
; (generate-digit) ;; you-fell-off-the-end

; (equal? 0 (generate-digit)) ;; 0
; (equal? 1 (generate-digit)) ;; 1
; (equal? 2 (generate-digit)) ;; 2

; ; (define res (generate-digit))
; ; (displayln res)

; (equal? 'you-fell-off-the-end (generate-digit)) ;; you-fell-off-the-end
; (equal? 'you-fell-off-the-end (generate-digit)) ;; you-fell-off-the-end



