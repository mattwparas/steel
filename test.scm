; ; eval takes an expression and an environment to a value
; (define (eval' e env) (cond
;   ((symbol? e)       (cadr (assq e env)))
;   ((eq? (car e) 'λ)  (cons e env))
;   (else              (apply (eval' (car e) env) (eval' (cadr e) env)))))

; ; apply takes a function and an argument to a value
; (define (apply f x)
;   (eval' (cddr (car f)) (cons (list (cadr (car f)) x) (cdr f))))

; ; read and parse stdin, then evaluate:
; (display (eval' (read) '())) (newline)


(define (hofstadter-male-female n)
  (letrec* ((female (lambda (n)
		     (if (= n 0)
			 1
			 (- n (male (female (- n 1)))))))
	   (male (lambda (n)
		   (if (= n 0)
		       0
		       (- n (female (male (- n 1))))))))
    (let loop ((i 0))
      (if (> i n)
	  '()
	  (cons (cons (female i)
		      (male i))
		(loop (+ i 1)))))))

(hofstadter-male-female 8)