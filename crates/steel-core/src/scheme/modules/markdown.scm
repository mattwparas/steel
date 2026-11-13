;; steel/markdown: renders markdown, the docs `help` shows, for a terminal.
;; `help` requires this the first time it runs and prints docs through
;; render-doc.
;;
;; Line by line, keeping the docs' own line breaks: fenced code (Steel code
;; coloured), headers, bullets and numbered lists with a hanging indent,
;; quotes, rules, tables as they are, and `code`, **bold**, *italic* inline.
;; Lines longer than the terminal wrap at spaces.

(require-builtin #%private/steel/text)

(provide render-markdown
         render-doc)

(define esc (string (integer->char 27)))

(define (style code text color?)
  (if (and code color?)
      (string-append esc "[" code "m" text esc "[0m")
      text))

(define inline-code-style "36")
(define bold-style "1")
(define italic-style "3")

;;;; Inline

;; Where `wanted` (a list of chars) next starts in `chars`, or #f
(define (find-chars chars wanted)
  (let loop ([chars chars] [at 0])
    (cond
      [(null? chars) #f]
      [(starts-with-chars? chars wanted) at]
      [else (loop (cdr chars) (+ at 1))])))

(define (starts-with-chars? chars wanted)
  (cond
    [(null? wanted) #t]
    [(null? chars) #f]
    [(equal? (car chars) (car wanted)) (starts-with-chars? (cdr chars) (cdr wanted))]
    [else #f]))

(define (take-chars chars count)
  (if (= count 0)
      '()
      (cons (car chars) (take-chars (cdr chars) (- count 1)))))

;; An emphasis marker opens before a non-space and closes after one
(define (closing-star chars)
  (let loop ([chars chars] [at 0] [previous #f])
    (cond
      [(null? chars) #f]
      [(and (equal? (car chars) #\*) previous (not (char-whitespace? previous)) (> at 0)) at]
      [else (loop (cdr chars) (+ at 1) (car chars))])))

;; The line as (text style) runs, style an SGR code or #f
(define (inline-runs line)
  (let loop ([chars (string->list line)] [plain '()] [runs '()] [previous #f])
    (define (with-plain)
      (if (null? plain)
          runs
          (cons (list (list->string (reverse plain)) #f) runs)))
    (cond
      [(null? chars) (reverse (with-plain))]
      ;; \* and friends are just the character
      [(and (equal? (car chars) #\\)
            (pair? (cdr chars))
            (not (char-alphabetic? (cadr chars)))
            (not (char-numeric? (cadr chars))))
       (loop (cddr chars) (cons (cadr chars) plain) runs (cadr chars))]
      [(equal? (car chars) #\`)
       (let ([end (find-chars (cdr chars) (list #\`))])
         (if end
             (loop (list-tail (cdr chars) (+ end 1))
                   '()
                   (cons (list (list->string (take-chars (cdr chars) end)) inline-code-style)
                         (with-plain))
                   #\`)
             (loop (cdr chars) (cons #\` plain) runs #\`)))]
      [(and (starts-with-chars? chars (list #\* #\*))
            (pair? (cddr chars))
            (not (char-whitespace? (caddr chars))))
       (let ([end (find-chars (cddr chars) (list #\* #\*))])
         (if (and end (> end 0))
             (loop (list-tail (cddr chars) (+ end 2))
                   '()
                   (cons (list (list->string (take-chars (cddr chars) end)) bold-style) (with-plain))
                   #\*)
             (loop (cdr chars) (cons #\* plain) runs #\*)))]
      [(and (equal? (car chars) #\*)
            (or (not previous) (char-whitespace? previous) (member previous '(#\( #\[)))
            (pair? (cdr chars))
            (not (char-whitespace? (cadr chars))))
       (let ([end (closing-star (cdr chars))])
         (if end
             (loop (list-tail (cdr chars) (+ end 1))
                   '()
                   (cons (list (list->string (take-chars (cdr chars) end)) italic-style) (with-plain))
                   #\*)
             (loop (cdr chars) (cons #\* plain) runs #\*)))]
      [else (loop (cdr chars) (cons (car chars) plain) runs (car chars))])))

;; Runs as words to wrap: (text style space-before?). Code spans stay whole.
(define (runs->words runs)
  (let loop ([runs runs] [words '()] [space #f])
    (if (null? runs)
        (reverse words)
        (let* ([run (car runs)]
               [text (car run)]
               [code (cadr run)])
          (if (equal? code inline-code-style)
              (loop (cdr runs) (cons (list text code space) words) #f)
              (let split ([chars (string->list text)] [current '()] [words words] [space space])
                (cond
                  [(null? chars)
                   (if (null? current)
                       (loop (cdr runs) words space)
                       (loop (cdr runs)
                             (cons (list (list->string (reverse current)) code space) words)
                             #f))]
                  [(char-whitespace? (car chars))
                   (if (null? current)
                       (split (cdr chars) '() words #t)
                       (split (cdr chars)
                              '()
                              (cons (list (list->string (reverse current)) code space) words)
                              #t))]
                  [else (split (cdr chars) (cons (car chars) current) words space)])))))))

;; Words into lines no wider than `width`, the first starting with `first`,
;; the rest with `rest`. A word longer than a line gets one to itself.
(define (wrap words width first rest color?)
  (let loop ([words words]
             [line first]
             [used (string-display-width first)]
             [empty #t]
             [lines '()])
    (if (null? words)
        (reverse (cons line lines))
        (let* ([word (car words)]
               [text (car word)]
               [gap (if (and (caddr word) (not empty)) 1 0)]
               [size (string-display-width text)])
          (if (and (not empty) (> (+ used gap size) width))
              (loop words rest (string-display-width rest) #t (cons line lines))
              (loop (cdr words)
                    (string-append line (if (= gap 1) " " "") (style (cadr word) text color?))
                    (+ used gap size)
                    #f
                    lines))))))

(define (render-inline text width first rest color?)
  (wrap (runs->words (inline-runs text)) width first rest color?))

;;;; Code

(define (code-style kind)
  (cond
    [(member kind '(form boolean)) "95"]
    [(equal? kind 'number) "93"]
    [(member kind '(string char)) "92"]
    [(equal? kind 'comment) "90"]
    [else #f]))

(define (highlight-code text color?)
  (if (not color?)
      text
      (let loop ([tokens (tokenize text)] [at 0] [out '()])
        (if (null? tokens)
            (apply string-append (reverse (cons (substring text at (string-length text)) out)))
            (let* ([token (car tokens)]
                   [code (code-style (car token))]
                   [start (cadr token)]
                   [end (caddr token)])
              (if (or (not code) (< start at))
                  (loop (cdr tokens) at out)
                  (loop (cdr tokens)
                        end
                        (cons (style code (substring text start end) #t)
                              (cons (substring text at start) out)))))))))

(define (steel-code? language)
  (member language '("" "scheme" "steel" "racket" "lisp")))

;; Indented four, coloured when it's Steel. Each line on its own (no
;; wrapping), but coloured as a block so strings and comments carry across.
(define (render-code lines language color?)
  (define text (string-join lines "\n"))
  (define shown
    (if (steel-code? language)
        (highlight-code text color?)
        text))
  (map (lambda (line) (string-append "    " line)) (split-many shown "\n")))

;;;; Blocks

(define (leading-spaces line)
  (let loop ([at 0])
    (if (and (< at (string-length line)) (equal? (string-ref line at) #\space))
        (loop (+ at 1))
        at)))

(define (heading-level text)
  (let loop ([at 0])
    (cond
      [(and (< at (string-length text)) (equal? (string-ref text at) #\#)) (loop (+ at 1))]
      [(and (> at 0)
            (<= at 6)
            (< at (string-length text))
            (equal? (string-ref text at) #\space))
       at]
      [else #f])))

(define (bullet? text)
  (and (>= (string-length text) 2)
       (member (string-ref text 0) '(#\* #\- #\+))
       (equal? (string-ref text 1) #\space)))

;; "12. " -> 4, the length of the marker
(define (number-marker text)
  (let loop ([at 0])
    (cond
      [(and (< at (string-length text)) (char-numeric? (string-ref text at))) (loop (+ at 1))]
      [(and (> at 0)
            (< (+ at 1) (string-length text))
            (equal? (string-ref text at) #\.)
            (equal? (string-ref text (+ at 1)) #\space))
       (+ at 2)]
      [else #f])))

(define (rule? text)
  (and (>= (string-length text) 3)
       (let ([first (string-ref text 0)])
         (and (member first '(#\- #\* #\_))
              (let loop ([chars (string->list text)])
                (cond
                  [(null? chars) #t]
                  [(or (equal? (car chars) first) (equal? (car chars) #\space)) (loop (cdr chars))]
                  [else #f]))))))

(define (render-block line width color?)
  (define indent (leading-spaces line))
  (define pad (make-string indent #\space))
  (define text (substring line indent (string-length line)))
  (define level (heading-level text))
  (define marker (number-marker text))
  (cond
    [(equal? text "") (list "")]
    [level
     ;; The whole heading in one style
     (map (lambda (row) (style (if (= level 1) "1;4" "1") row color?))
          (render-inline (substring text (+ level 1) (string-length text)) width pad pad #f))]
    [(rule? text) (list (style "90" (string-append pad (make-string (max 3 (- width indent)) #\─)) color?))]
    [(bullet? text)
     (render-inline (substring text 2 (string-length text))
                    width
                    (string-append pad "• ")
                    (string-append pad "  ")
                    color?)]
    [marker
     (render-inline (substring text marker (string-length text))
                    width
                    (string-append pad (substring text 0 marker))
                    (string-append pad (make-string marker #\space))
                    color?)]
    [(starts-with? text ">")
     (let ([bar (style "90" "│ " color?)])
       (render-inline (trim-start (substring text 1 (string-length text)))
                      width
                      (string-append pad bar)
                      (string-append pad bar)
                      color?))]
    [(starts-with? text "|") (list line)]
    [else (render-inline text width pad pad color?)]))

;; The fence's language, or #f when `line` isn't a fence
(define (fence-language line)
  (define text (trim line))
  (and (starts-with? text "```") (trim (substring text 3 (string-length text)))))

;; `text` ready to print on a terminal `columns` wide, with escapes when
;; `color?`
(define (render-markdown text [columns 80] [color? #t])
  (define width (max 20 (- columns 1)))
  (let loop ([lines (split-many text "\n")] [out '()])
    (if (null? lines)
        (string-join (reverse out) "\n")
        (let ([language (fence-language (car lines))])
          (if language
              ;; Up to the closing fence, or the end if there isn't one
              (let code ([rest (cdr lines)] [block '()])
                (cond
                  [(null? rest)
                   (loop '() (append (reverse (render-code (reverse block) language color?)) out))]
                  [(fence-language (car rest))
                   (loop (cdr rest)
                         (append (reverse (render-code (reverse block) language color?)) out))]
                  [else (code (cdr rest) (cons (car rest) block))]))
              (loop (cdr lines) (append (reverse (render-block (car lines) width color?)) out)))))))

;; What help prints: rendered to fit the terminal, coloured when stdout is one
(define (render-doc doc)
  (render-markdown doc (or (terminal-columns) 80) (stdout-terminal?)))
