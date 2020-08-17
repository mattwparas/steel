(export alex-is-the-best)

(define alex-is-the-best "boop boop")
(define boop-boop "alex-is-the-best")






(λ (x y) (+ 1 2 3))



(defn (take lst n)
  (defn (loop x l acc)
    (if (= x 0)
        acc
        (loop (- x 1) (cdr l) (cons (car l) acc))))
  (loop n lst (list)))

;; TODO
;; One problem with the way I label indices is that shadowed values at the top level
;; Make the new definitions clobber the old ones, call std::mem::drop on it
;;
;; also make the new ones clobber the old ones so that basically _everything_ gets moved up?
;; binds should also just add instead of add_or_get? maybe idk who cares really honestly
;; oh well this is so stupid hard...
;; Not sure why i'm not using a hashmap but here we are

(map' (fn (x) ...) lst)

;; add hashmaps?
;; also identify ways we can merge the two
;; This is pretty neat



(-> (range 0 100))


;; (filter-map)

(define merge-lists
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (if (< (car l1) (car l2))
                (cons (car l1) (merge-lists (cdr l1) l2))
                (cons (car l2) (merge-lists (cdr l2) l1)))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in even positions

(define even-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            '()
            (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;;; -------------------------------------------------------------------
;;; Given list l, output those tokens of l which are in odd positions

(define odd-numbers
  (lambda (l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            (list (car l))
            (cons (car l) (odd-numbers (cdr (cdr l))))))))

;;; ---------------------------------------------------------------------
;;; Use the procedures above to create a simple and efficient merge-sort

(define merge-sort
  (lambda (l)
    (if (null? l)
        l
        (if (null? (cdr l))
            l
            (merge-lists
             (merge-sort (odd-numbers l))
             (merge-sort (even-numbers l)))))))

(define p "/Users/mwparas/Documents/MatSci201")




;; (define (tree-rec path)
;;   (cond [(is-file? path)
;;          ;; (display "|----")
;;          (displayln (file-name path))]
;;         [(is-dir? path)
;;          ;; (display "|---")
;;          (displayln (file-name path))
;;          (map (fn (x)
;;                 (display "|---")
;;                 (tree-rec x))
;;               (merge-sort (read-dir path)))]
;;         [else (displayln "something went wrong!")]))

;; Closure offset is getting calculated incorrectly
;; This seems to be due to the env getting bloated for some reason
;; the length just increases endlessly... check the reserve_defs?


(define (tree-rec path padding)
  (cond [(is-file? path)
         (display padding)
         (displayln (file-name path))]
        [(is-dir? path)
         (display padding)
         (displayln (file-name path))
         (map (fn (x)
                (tree-rec x (string-append padding "    ")))

              (merge-sort (read-dir path)))]
        [else void]))


(tree-rec p "")

(define (tree p)
  (tree-rec p ""))

;; (define (tree path)
;;   (pretty-print (tree-rec path)))

;; (tree p)


(define (tree path)
  (displayln path)
  (map tree-rec
       (merge-sort (read-dir path))))

(tree p)

(iter lst
      map (fn (x) 10)
      filter even?


      -> lst)

(defn (take-vec vec n)
  (defn (loop x l acc)
    (if (= x 0)
        acc
        (loop (- x 1) (vec-rest l) (push-front (pop-front l) acc))))
  (loop n vec (vector)))


(define (run n)
  (define (loop i sum)
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum))))
  (loop n 0))


(define (filter pred lst)
  (if (empty? lst)
      '()
      (filter' pred lst)))

(define (map func lst)
  (if (empty lst)
      '()
      (map' func lst)))

(define (test)
  (define x (lambda (w) (lambda (x y) (lambda (z) (lambda (a)
                                                    (display "Last lambda")
                                                    (newline)
                                                    (+ w x y z a))))))
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5)
  ((((x 1) 2 3) 4) 5))

(define test
  (lambda ()
    (define z #true)
    ((lambda (##z)
       (if ##z ##z
           ((lambda (##z)
              (if ##z ##z z)) #false)))
     #false)))

(define test
  (lambda ()
    (define z \"hello\")
    ((lambda (##z)
       (if ##z ##z
           ((lambda (##z)
              (if ##z ##z z))
            3)))
     2)))

(define test
  (lambda ()
    (define z "hello")
    ((lambda (y)
       (if y y
           ((lambda (y)
              (if y y z))
            3)))
     2)))

(define (take lst n)
  (define (loop x l accum)
    (if (= x 0)
        accum
        (loop (- x 1) (cdr l) (cons (car l) accum))))
  (loop n lst (list)))


(define take (lambda (lst n)
               (define loop (lambda (x l accum)
                              (if (or (= x 0) (null? l))
                                  accm
                                  (loop (sub1 x) (cdr l) (cons (car l) accum)))))
               (loop n lst empty)))

(define take (lambda (lst n)
               (define loop (lambda (x l accum)
                              (if (= x 0)
                                  accum
                                  (loop (- x 1) (cdr l) (cons (car l) accum)))))
               (loop n lst (list))))


(defn (take lst n))



("MatSci201"
 ("Midterm #1"
  ("Assessment Solutions"
   "MatSci201-Midterm1-PracticeExam-F19-Sol.pdf"
   "MatSci201-Midterm1-PracticeExam-F19.pdf"
   "PSet-R1-Sol.pdf"
   "PSetD2-F19-Sol.pdf"
   "PSetD3-F19-Sol.pdf"
   "PSetD4-F19-Sol.pdf"
   "PracticeD2_F19-Sol.pdf"
   "PracticeD3_F19-Sol.pdf"
   "QuizD2_F19-Sol.pdf")
  ("Lectures"
   "L1-1_Introduction_to_Materials-F19.pdf"
   "L1-2-Bonding-F19.pdf"
   "L2-1_Crystal Structure I-F19.pdf"
   "L2-2 Crystal Structure II-F19-Companion.pdf"
   "L2-2 Crystal Structure II-F19.pdf"
   "L3-1 Ceramics and Polymer Structures-F19.pdf")
  "Midterm1_StudyGuide-F19.pdf"))



(define (pretty-print lst)
  (cond [(string? lst) lst]
        [(list? lst)



         (displayln (car lst))
         (map pretty-print (cdr lst))]
        [else void]))

'("MatSci201"
  (
   ("Midterm #1"
    (
     ("Assessment Solutions"
      ("MatSci201-Midterm1-PracticeExam-F19-Sol.pdf"
       "MatSci201-Midterm1-PracticeExam-F19.pdf"
       "PSet-R1-Sol.pdf"
       "PSetD2-F19-Sol.pdf"
       "PSetD3-F19-Sol.pdf"
       "PSetD4-F19-Sol.pdf"
       "PracticeD2_F19-Sol.pdf"
       "PracticeD3_F19-Sol.pdf"
       "QuizD2_F19-Sol.pdf")
      )
     ("Lectures"
      ("L1-1_Introduction_to_Materials-F19.pdf"
       "L1-2-Bonding-F19.pdf"
       "L2-1_Crystal Structure I-F19.pdf"
       "L2-2 Crystal Structure II-F19-Companion.pdf"
       "L2-2 Crystal Structure II-F19.pdf"
       "L3-1 Ceramics and Polymer Structures-F19.pdf")
      )
     "Midterm1_StudyGuide-F19.pdf")
    )
   )
  )
