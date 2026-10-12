use crate::steel_vm::engine::Engine;

const PROGRAM: &str = r#"
(define (capture) (call/cc (lambda (k) k)))

;; A let local, with the continuation captured in a called function.
(define (let-local x)
  (let ([n 0])
    (let ([k (capture)])
      (set! n (+ n 1))
      (if (< n 4) (k k) (* x n)))))
(assert! (= 20 (let-local 5)))

;; The continuation captured inline.
(define (captured-here x)
  (let ([n 0])
    (let ([k (call/cc (lambda (k) k))])
      (set! n (+ n 1))
      (if (< n 4) (k k) (* x n)))))
(assert! (= 20 (captured-here 5)))

;; A parameter.
(define (parameter x n)
  (define k (capture))
  (set! n (+ n 1))
  (if (< n 4) (k k) (* x n)))
(assert! (= 20 (parameter 5 0)))

;; Internal defines.
(define (defined x)
  (define n 0)
  (define k (capture))
  (set! n (+ n 1))
  (if (< n 4) (k k) (* x n)))
(assert! (= 20 (defined 5)))

;; An unassigned binding around the local.
(define (under-another a)
  (let ([b a])
    (let ([n 0])
      (let ([k (capture)])
        (set! n (+ n 1))
        (if (< n 4) (k k) (* b n))))))
(assert! (= 20 (under-another 5)))

;; Internal defines in a body of their own.
(define (defined-in-a-let x)
  (let ()
    (define n 0)
    (define k (capture))
    (set! n (+ n 1))
    (if (< n 4) (k k) (* x n))))
(assert! (= 20 (defined-in-a-let 5)))

;; A local that a closure also captures.
(define (closed-over x)
  (let ([n 0])
    (let ([k (capture)])
      (let ([read (lambda () n)])
        (set! n (+ n 1))
        (if (< (read) 4) (k k) (* x (read)))))))
(assert! (= 20 (closed-over 5)))

;; One local assigned before the capture and one after.
(define (two-locals)
  (let ([before 0] [after 0])
    (set! before (+ before 10))
    (let ([k (capture)])
      (set! after (+ after 1))
      (set! before (+ before 1))
      (if (< after 3) (k k) (list before after)))))
(assert! (equal? (list 13 3) (two-locals)))

;; Entered again from outside the function.
(define saved #f)
(define (counts-entries)
  (let ([n 0])
    (call/cc (lambda (k) (set! saved k)))
    (set! n (+ n 1))
    n))
(define (enters-three-times)
  (let ([entries (box '())])
    (set-box! entries (cons (counts-entries) (unbox entries)))
    (when (< (length (unbox entries)) 3) (saved void))
    (unbox entries)))
(assert! (equal? (list 3 2 1) (enters-three-times)))

;; Plain assignment still works.
(define (sum-to limit)
  (let ([total 0] [i 0])
    (let loop ()
      (when (< i limit)
        (set! total (+ total i))
        (set! i (+ i 1))
        (loop)))
    total))
(assert! (= 4950 (sum-to 100)))
(define (swap a b)
  (let ([held a])
    (set! a b)
    (set! b held)
    (list a b)))
(assert! (equal? (list 2 1) (swap 1 2)))
"#;

#[test]
fn assigned_locals_keep_what_was_assigned_when_a_continuation_is_entered_again() {
    if !crate::config::config().box_assigned_locals {
        return;
    }

    Engine::new()
        .run(PROGRAM)
        .expect("each local holds what was last assigned to it");
}

#[test]
fn assigned_locals_keep_what_was_assigned_in_a_module() {
    if !crate::config::config().box_assigned_locals {
        return;
    }

    let mut engine = Engine::new();
    engine.register_steel_module("assigned-locals".to_owned(), PROGRAM.to_owned());
    engine
        .run(r#"(require "assigned-locals")"#)
        .expect("each local holds what was last assigned to it");
}
