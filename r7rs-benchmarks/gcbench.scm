(require "common.scm")

;;  This is adapted from a benchmark written by John Ellis and Pete Kovac
;;  of Post Communications.
;;  It was modified by Hans Boehm of Silicon Graphics.
;;  It was translated into Scheme by William D Clinger of Northeastern Univ.
;;  Last modified 24 November 2007 (translated into R6RS Scheme).
;;
;;       This is no substitute for real applications.  No actual application
;;       is likely to behave in exactly this way.  However, this benchmark was
;;       designed to be more representative of real applications than other
;;       Java GC benchmarks of which we are aware.
;;       It attempts to model those properties of allocation requests that
;;       are important to current GC techniques.
;;       It is designed to be used either to obtain a single overall performance
;;       number, or to give a more detailed estimate of how collector
;;       performance varies with object lifetimes.  It prints the time
;;       required to allocate and collect balanced binary trees of various
;;       sizes.  Smaller trees result in shorter object lifetimes.  Each cycle
;;       allocates roughly the same amount of memory.
;;       Two data structures are kept around during the entire process, so
;;       that the measured performance is representative of applications
;;       that maintain some live in-memory data.  One of these is a tree
;;       containing many pointers.  The other is a large array containing
;;       double precision floating point numbers.  Both should be of comparable
;;       size.
;;
;;       The results are only really meaningful together with a specification
;;       of how much memory was used.  It is possible to trade memory for
;;       better time performance.  This benchmark should be run in a 32 MB
;;       heap, though we don't currently know how to enforce that uniformly.

;; In the Java version, this routine prints the heap size and the amount
;; of free memory.  There is no portable way to do this in Scheme ; each
;; implementation needs its own version.

(define (run-benchmark2 name thunk)
  (display name)
  (newline)
  (thunk))

(define (PrintDiagnostics)
  (display " Total memory available= ???????? bytes")
  (display "  Free memory= ???????? bytes")
  (newline))

(define (gcbench kStretchTreeDepth)

  ;;  Nodes used by a tree of a given size
  (define (TreeSize i)
    (- (expt 2 (+ i 1)) 1))

  ;;  Number of iterations to use for a given tree depth
  (define (NumIters i)
    (quotient (* 2 (TreeSize kStretchTreeDepth))
              (TreeSize i)))

  ;;  Parameters are determined by kStretchTreeDepth.
  ;;  In Boehm's version the parameters were fixed as follows:
  ;;    public static final int kStretchTreeDepth    = 18 ;  // about 16Mb
  ;;    public static final int kLongLivedTreeDepth  = 16 ;  // about 4Mb
  ;;    public static final int kArraySize  = 500000 ;       // about 4Mb
  ;;    public static final int kMinTreeDepth = 4 ;
  ;;    public static final int kMaxTreeDepth = 16 ;
  ;;  In Larceny the storage numbers above would be 12 Mby, 3 Mby, 6 Mby.

  (let* ((kLongLivedTreeDepth (- kStretchTreeDepth 2))
         (kArraySize          (* 4 (TreeSize kLongLivedTreeDepth)))
         (kMinTreeDepth       4)
         (kMaxTreeDepth       kLongLivedTreeDepth))

    ;; Elements 3 and 4 of the allocated records are useless.
    ;; They're just to take up space, so this will be comparable
    ;; to the Java original.

    ;; Steel has no define-record-type ; its native mutable struct is the
    ;; closest equivalent, and keeps this an allocation benchmark rather
    ;; than a benchmark of a record shim.
    (struct classNode (left right i j) #:mutable)

    (define make-node-raw classNode)
    (define node.left classNode-left)
    (define node.right classNode-right)
    (define node.i classNode-i)
    (define node.j classNode-j)
    (define node.left-set! set-classNode-left!)
    (define node.right-set! set-classNode-right!)
    (define node.i-set! set-classNode-i!)
    (define node.j-set! set-classNode-j!)

    (let ((make-empty-node (lambda () (make-node-raw 0 0 0 0)))
	  (make-node (lambda (l r) (make-node-raw l r 0 0))))

      ;;  Build tree top down, assigning to older objects.
      (define (Populate iDepth thisNode)
        (if (<= iDepth 0)
            #f
            (let ((iDepth (- iDepth 1)))
              (node.left-set! thisNode (make-empty-node))
              (node.right-set! thisNode (make-empty-node))
              (Populate iDepth (node.left thisNode))
              (Populate iDepth (node.right thisNode)))))

      ;;  Build tree bottom-up
      (define (MakeTree iDepth)
        (if (<= iDepth 0)
            (make-empty-node)
            (make-node (MakeTree (- iDepth 1))
                       (MakeTree (- iDepth 1)))))

      (define (TimeConstruction depth)
        (let ((iNumIters (NumIters depth)))
          (display (string-append "Creating "
                                  (number->string iNumIters)
                                  " trees of depth "
                                  (number->string depth)))
          (newline)
          (run-benchmark2
           "GCBench: Top down construction"
           (lambda ()
             (do ((i 0 (+ i 1)))
                 ((>= i iNumIters))
               (Populate depth (make-empty-node)))))
          (run-benchmark2
           "GCBench: Bottom up construction"
           (lambda ()
             (do ((i 0 (+ i 1)))
                 ((>= i iNumIters))
               (MakeTree depth))))))

      (define (run-benchmark-x)
        (display "Garbage Collector Test")
        (newline)
        (display (string-append
                  " Stretching memory with a binary tree of depth "
                  (number->string kStretchTreeDepth)))
        (newline)
        (PrintDiagnostics)
        (run-benchmark2
         "GCBench: Main"
         (lambda ()
           ;;  Stretch the memory space quickly
           (MakeTree kStretchTreeDepth)

           ;;  Create a long lived object
           (display (string-append
                     " Creating a long-lived binary tree of depth "
                     (number->string kLongLivedTreeDepth)))
           (newline)
           (let ((longLivedTree (make-empty-node)))
             (Populate kLongLivedTreeDepth longLivedTree)

             ;;  Create long-lived array, filling half of it
             (display (string-append
                       " Creating a long-lived array of "
                       (number->string kArraySize)
                       " inexact reals"))
             (newline)
             (let ((array (make-vector kArraySize 0.0)))
               (do ((i 0 (+ i 1)))
                   ((>= i (quotient kArraySize 2)))
                 (vector-set! array i (/ 1.0 (inexact (+ i 1)))))
               (PrintDiagnostics)

               (do ((d kMinTreeDepth (+ d 2)))
                   ((> d kMaxTreeDepth))
                 (TimeConstruction d))

               (if (or (eq? longLivedTree '())
                       (let ((n (min 1000
                                     (- (quotient (vector-length array)
                                                  2)
                                        1))))
                         (not (= (vector-ref array n)
                                 (/ 1.0 (inexact (+ n 1)))))))
                   (begin (display "Failed") (newline)))
               ;;  fake reference to LongLivedTree
               ;;  and array
               ;;  to keep them from being optimized away
               ))))
        (PrintDiagnostics))

      (run-benchmark-x))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "gcbench"))
    (display "The garbage collector should touch about ")
    (display (expt 2 (- input1 13)))
    (display " megabytes of heap storage.")
    (newline)
    (display "The use of more or less memory will skew the results.")
    (newline)
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (gcbench (hide count input1)))
     (lambda (result) #t))))

(with-input-from-file (bench-input "gcbench") run-benchmark)
