(require "steel/sync")
(require "steel/time/time.scm")
(require-builtin steel/time)

;; Dedicated internal thread pool - sits and waits for things.
;; How does eval on another thread behave?
;; The ultimate question...

;; Thread pool for parallel map - will just be static for all pmaps.
(define tp (make-thread-pool 16))

;; Chunk a list into the components?
; (define (chunk))

;; Convert list into chunks that it can operate on, independently - since the
;; list is already stored as a bunch of exponential things in a row, we can
;; slice it up into those pieces nicely - for now, we can just assume
;; that this is something we _could_ implement, and _should_ implement, but I
;; don't feel like going through that exercise right now.

(define printer-lock (mutex))
(define (instant/elapsed->string t)
  (~> t (instant/elapsed) (duration->string)))

(define (pmap func lst)
  ;; Chunk based on the natural growth
  (define chunks (list-chunks lst))
  ; (displayln (map length chunks))
  ;; Lots of additional allocation?
  (define tasks
    (map (lambda (chunk)
           (submit-task tp
                        (lambda ()
                          (define now (instant/now))
                          ;; Find out where the overhead is coming from
                          (define res (map func chunk))

                          ; (displayln printer-lock)

                          ; (lock! printer-lock
                          ;        (lambda ()
                          ;          (displayln (thread::current/id)
                          ;                     "-"
                          ;                     "Chunk size:"
                          ;                     (length chunk)
                          ;                     ":"
                          ;                     (instant/elapsed->string now))))

                          res)))
         chunks))
  ;; Reducing contention... how to do it? Probably need to do some kind of work with
  ;; making sure that the globals don't get locked up - I'm guessing that is where most of
  ;; the wait time here is - if each thread can get its own copies of the values, then
  ;; they don't have to be locked up reading a global.
  (transduce tasks (flat-mapping (lambda (x) (block-on-task x))) (into-list)))

;; Why is this so slow?
(define inputs (range 0 100000))

(define (looper x)
  (if (= x 100) x (looper (+ x 1))))

(define (expensive-add1 x)
  (looper 0)
  (add1 x))

;; List chunks -> Most elegant way of submitting the values in the list?
;; Also, more or less guaranteed to be sequential in memory. Big memory savings by
;; putting things together.

;; Where is the contention?
;; Contention on function calls.
(for-each (lambda (_)
            ;; Rooted instructions - very important.
            ;; Need to have a lot more tests around this.
            (displayln (equal? (time! (pmap expensive-add1 inputs))
                               (time! (map expensive-add1 inputs)))))
          (range 0 10))

; (time! (pmap expensive-add1 inputs))
; (time! (pmap expensive-add1 inputs))
; (time! (map expensive-add1 inputs))

; (define (test)
;   (dynamic-wind (lambda () (displayln "before"))
;                 (lambda () (displayln "during"))
;                 (lambda () (displayln "after")))
;   10)

; (displayln (test))
