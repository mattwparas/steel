(require "steel/sync")
(require "steel/time/time.scm")

;; Thread pool for parallel map - will just be static for all pmaps.
(define tp (make-thread-pool 16))

;; Chunk a list into the components?
; (define (chunk))

;; Convert list into chunks that it can operate on, independently - since the
;; list is already stored as a bunch of exponential things in a row, we can
;; slice it up into those pieces nicely - for now, we can just assume
;; that this is something we _could_ implement, and _should_ implement, but I
;; don't feel like going through that exercise right now.

(define (pmap func lst)
  ;; Chunk based on the natural growth
  (define chunks (list-chunks lst))
  (displayln (map length chunks))
  ;; Lots of additional allocation?
  (define tasks (map (lambda (chunk) (submit-task tp (lambda () (map func chunk)))) chunks))
  ;; Reducing contention... how to do it? Probably need to do some kind of work with
  ;; making sure that the globals don't get locked up - I'm guessing that is where most of
  ;; the wait time here is - if each thread can get its own copies of the values, then
  ;; they don't have to be locked up reading a global.
  (transduce tasks (into-for-each block-on-task)))

(define inputs (range 0 10000))

; (for-each (lambda (_)
(time! (pmap add1 inputs))
(time! (map add1 inputs))
; )
; (range 0 100))

(time! (pmap add1 inputs))
(time! (map add1 inputs))
