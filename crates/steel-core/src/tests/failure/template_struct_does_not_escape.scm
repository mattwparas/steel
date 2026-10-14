(define-syntax make-point
  (syntax-rules ()
    [(_) (struct point (x y))]))
(make-point)

(point-x (point 1 2))
