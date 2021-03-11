;;; Simple 'Tree' implementation in Steel
;;; Relies on the use of bindings to the file system through Rust
(require "sort.rkt")

(define (flatten x)
  (cond [(null? x) '()]
        [(not (list? x)) (list x)]
        [else (append (flatten (car x))
                      (flatten (cdr x)))]))


;; Simple tree implementation
;; Walks the file structure and prints without much fancy formatting
;; Returns a list of the visited files for convenience
(define (tree p)
  (define (tree-rec path padding)
    (define name (file-name path))
    (displayln (string-append padding name))
    (cond [(is-file? path) name]
          [(is-dir? path)
           (map (fn (x)
                  (tree-rec x (string-append padding "    ")))
                (merge-sort (read-dir path)))]
          [else void]))
  (flatten (tree-rec p "")))


; (define p "/Users/mwparas/Documents/MatSci201")

; (tree p)

;; MatSci201
;;     Midterm #1
;;         Assessment Solutions
;;             MatSci201-Midterm1-PracticeExam-F19-Sol.pdf
;;             MatSci201-Midterm1-PracticeExam-F19.pdf
;;             PSet-R1-Sol.pdf
;;             PSetD2-F19-Sol.pdf
;;             PSetD3-F19-Sol.pdf
;;             PSetD4-F19-Sol.pdf
;;             PracticeD2_F19-Sol.pdf
;;             PracticeD3_F19-Sol.pdf
;;             QuizD2_F19-Sol.pdf
;;         Lectures
;;             L1-1_Introduction_to_Materials-F19.pdf
;;             L1-2-Bonding-F19.pdf
;;             L2-1_Crystal Structure I-F19.pdf
;;             L2-2 Crystal Structure II-F19-Companion.pdf
;;             L2-2 Crystal Structure II-F19.pdf
;;             L3-1 Ceramics and Polymer Structures-F19.pdf
;;         Midterm1_StudyGuide-F19.pdf
