; #lang racket

; Generic implementation of all four delimited control operators
;    shift/reset, prompt/control, shift0/reset0 and prompt0/control0
;                   aka. -F- through +F+   
;
; The code below is parameterized by two boolean flags:
; is-shift and keep-delimiter-upon-effect.
; Each flag is present in the code exactly once, in a trivial
; context. In particular, the difference between shift and control is
; one statement: (hole-push! (cell-new k-return is-shift))
; which tells whether the created hole is delimiting or not.
; All four combinations of the two flags correspond to four
; delimited control operators,  -F- through +F+
;
; The code relies on call/cc for capturing undelimited
; continuations, and uses one global mutable cell. It turns out, this
; is sufficient for implementing not only shift/reset (Danvy/Filinski)
; but also for control/prompt and the other F operators. That has not
; been known before. In particular, the implementation of
; control/prompt needs no eq? operations.
;
; This implementation immediately leads to a CPS transform for
; control/prompt (which has not been known before either). That
; transform turns almost identical to the one discussed in the
; forth-coming paper ``A Monadic Framework for Delimited
; Continuations'' by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry
;
; This code is inspired by CC_Ref.hs -- contexts with holes, while reading
; Dorai Sitaram and Matthias Felleisen paper (Lisp and symbolic computation,
; 1990)
; A hole has a continuation and a mark. The mark, if #t, says if the hole
; is a delimiting hole.
; Non-delimiting hole is just like the return from a regular function.
;
; $Id: delim-control-n.scm 815 2005-09-05 23:02:12Z oleg $

; Two parameterizing boolean flags. We can change them at run-time
; and so mutate shift into control at run-time! The regression test code below
; does exactly that, so it can test all four *F* operators.
(define is-shift #f)
(define keep-delimiter-upon-effect #t)

; This is one single global mutable cell
(define holes '())
(define (hole-push! hole) (set! holes (cons hole holes)))
(define (hole-pop!) (let ((hole (car holes))) (set! holes (cdr holes)) hole))

(define (cell-new v mark) (cons v mark))
(define (cell-ref c) (car c))
(define (cell-marked? c) (cdr c))

; Essentially this is the ``return from the function''
(define (abort-top! v) ((cell-ref (hole-pop!)) v))
(define (unwind-till-marked!)
  (when (null? holes) (error "No prompt set"))
  (let ((hole (hole-pop!)))
    (if (cell-marked? hole)		; if marked, it's prompt's hole
      (begin (hole-push!		; put it back
	       (if keep-delimiter-upon-effect hole
		 (cell-new (cell-ref hole) #f))) ; make the hole non-delimiting
	'())	
      (cons hole (unwind-till-marked!)))))

(define (prompt* thunk)
  (call-with-current-continuation
    (lambda (outer-k)
      (hole-push! (cell-new outer-k #t)) ; it's prompt's hole
      (abort-top! (thunk)))))
      
(define (control* f)
  (call-with-current-continuation
    (lambda (k-control)
      (let* ((holes-prefix (reverse (unwind-till-marked!)))
	     (invoke-subcont 
	       (lambda (v)
		 (call-with-current-continuation
		   (lambda (k-return)
		     (hole-push! (cell-new k-return is-shift))
		     (for-each hole-push! holes-prefix)
		     (k-control v))))))
	(abort-top! (f invoke-subcont))))))

(define (abort v) (control* (lambda (k) v)))

; Some syntactic sugar
(define-syntax prompt
  (syntax-rules ()
    ((prompt e) (prompt* (lambda () e)))))

(define-syntax control
  (syntax-rules ()
    ((control f e) (control* (lambda (f) e)))))


(define (void) void)

(define (for-each func lst)
    (if (null? lst) 
        void
        (begin
            (func (car lst))
            (if (null? lst) 
                (void)
                (for-each func (cdr lst))))))


;------------------------------------------------------------------------
;			Shift tests
(display "shift tests") (newline)

(set! is-shift #t)
(set! keep-delimiter-upon-effect #t)

; introduce convenient synonyms

(define-syntax reset
  (syntax-rules ()
    ((reset e) (prompt e))))

(define-syntax shift
  (syntax-rules ()
    ((shift f e) (control f e))))

(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (* 10 (reset (* 2 (shift g (reset 
				   (* 5 (shift f (+ (f 1) 1)))))))))
(newline)
; --> 60

(display (let ((f (lambda (x) (shift k (k (k x))))))
	   (+ 1 (reset (+ 10 (f 100))))))
(newline)
; --> 121

(display (reset
	   (let ((x (shift f (cons 'a (f '())))))
	     (shift g x))))
(newline)
; ==> '(a)

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(define (shift* p) (shift f (p f)))
(reset (display (let ((x 'abcde)) (eq? x ((shift* shift*) x)))))
(reset (let ((x 'abcde)) (eq? x ((shift* shift*) x))))
(newline)

(define visit
    (lambda (xs)
        (if (null? xs)
        '()
        (visit (control*
            (lambda (k)
                (cons (car xs) (k (cdr xs)))))))))

(define (traverse xs)
    (define visit
        (lambda (xs)
            (if (null? xs)
            '()
            (visit (control*
                (lambda (k)
                    (cons (car xs) (k (cdr xs)))))))))
      (prompt*
        (lambda ()
            (visit xs))))

(display "Ex by Olivier Danvy") (newline)
(display (traverse '(1 2 3 4 5)))
(newline)