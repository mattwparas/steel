(define x 0) ; dummy value - will be used to store continuation later

(+ 2 (call/cc 
          (lambda (cc)
           (set! x cc)  ; set x to the continuation cc; namely, (+ 2 _)
           3)))         ; returns 5

(x 4) ;; 6 