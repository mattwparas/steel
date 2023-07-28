(define (crunch composer lsts)
  (if (null? lsts) composer (crunch (compose composer (zipping (car lsts))) (cdr lsts))))

(define (map-many func lst . lsts)
  (if (null? lsts)
      (map func lst)
      ;; Handle the case for many lists
      (let ([composed-transducer (crunch (compose) lsts)])
        (transduce lst composed-transducer (mapping (lambda (x) (apply func x))) (into-list)))))
