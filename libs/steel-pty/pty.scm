(#%require-dylib "libsteel_pty"
                 (only-in create-native-pty-system!
                          kill-pty-process!
                          pty-process-send-command
                          async-try-read-line
                          make-ansi-tokenizer
                          tokenize-line))

(require "steel/result")

;; Code mapping
(define EraseToEndOfLine 0)
(define EraseToStartOfLine 1)
(define EraseLine 2)

;; Spawn the background thread with the pty process
;; communicate over the process?

(define (for-each func lst)
  (if (null? lst)
      void
      (begin
        (func (car lst))
        (when (null? lst)
          (return! void))
        (for-each func (cdr lst)))))

(define *pty-process* 'uninitialized)
(define *ansi-tokenizer* (make-ansi-tokenizer))
(define (start-terminal)
  (set! *pty-process* (create-native-pty-system!)))

(define (kill-terminal)
  (kill-pty-process! *pty-process*))

;; Appends the carriage return to the given command
(define (send-command command)
  (pty-process-send-command *pty-process* (string-append command "\r")))

(define escape-code-map
  (list ;; Mapping of index -> function
   ;; EraseToEndOfLine
   (lambda () void)
   ;; EraseToStartOfLine
   (lambda () void)
   ;; EraseLine
   (lambda () void)
   ;; Cursor Position escape sequence
   (lambda () void)))

;; Just spawn on another thread, communicate that way
(define (read-line)
  ;; Tokenize the resulting output
  ;; most characters just need to get written out
  (define tokenized
    (tokenize-line *ansi-tokenizer* (local-executor/block-on (async-try-read-line *pty-process*))))

  (for-each (lambda (line)
              (cond
                [(int? line) ((list-ref escape-code-map line))]

                ;; TODO: Not good, we don't need to allocate this much
                [(char? line) (display (string line))]
                [(string? line) (display line)]

                ; (write-line-to-terminal cx line)
                ; [line (helix.static.insert_string cx line)]
                [else void]))
            tokenized))

(skip-compile
 (define *pty-process* 'uninitialized)
 ;; Start at every 20 ms. At some point, we are going to be idled, and we don't want to constantly
 ;; be running in a loop to refresh. At this point we can just delay
 (define *DEFAULT-REFRESH-DELAY* 20)
 (define *terminal-refresh-delay* *DEFAULT-REFRESH-DELAY*)
 (define fail-check 0)
 (define (reset-fail-check!)
   (set! fail-check 0))
 (define (mark-failed!)
   (set! fail-check (+ 1 fail-check)))
 ;; TODO: Cap this to some large enough value
 ;; This could definitely cause issues for long running stuff...
 (define (increase-terminal-refresh-delay!)
   (set! *terminal-refresh-delay* (* *terminal-refresh-delay* *terminal-refresh-delay*)))
 (define (reset-terminal-refresh-delay!)
   (set! *terminal-refresh-delay* *DEFAULT-REFRESH-DELAY*))
 (provide initialize-pty-process)
 (define (initialize-pty-process)
   (set! *pty-process* (create-native-pty-system!)))
 (provide kill-terminal)
 (define (kill-terminal)
   (kill-pty-process! *pty-process*))
 (define *go-signal* #t)
 (provide interrupt-terminal)
 (define (interrupt-terminal)
   (set! *go-signal* #f)
   (enqueue-thread-local-callback (lambda () (set! *go-signal* #true))))
 (provide terminal-loop)
 (define (terminal-loop)
   (when *go-signal*
     (begin

       (let ([line-future (async-try-read-line *pty-process*)])
         (helix-await-callback line-future
                               (lambda (line)
                                 (async-write-from-terminal-loop cx line)

                                 (terminal-loop cx))))))))

;; Goes until there isn't any output to read, writing each line
(skip-compile (define (read-until-no-more-lines)
                (error! "TODO"))
              ; (let ([output (pty-process-try-read-line *pty-process*)])
              ;   (when output
              ;     (helix.static.insert_string cx output)
              ;     (read-until-no-more-lines cx))))
              (define fail-check 0)
              (define (write-line-to-terminal line)
                (temporarily-switch-focus (lambda ()
                                            ;; Open up the repl, write the output
                                            (open-labelled-buffer "steel-repl")
                                            (helix.static.insert_string line))))
              (define (write-char-to-terminal char)
                (cond
                  [(equal? char #\newline) (helix.static.insert_string "\n")]
                  [(equal? char #\return)
                   void
                   ; (temporarily-switch-focus cx
                   ;                           (lambda (cx)
                   ;                             ;; Open up the repl, write the output
                   ;                             (open-labelled-buffer cx "steel-repl")

                   ;                             (helix.static.insert_newline cx)
                   ;                             (helix.static.delete_selection cx)))
                   ]
                  [else (helix.static.insert_char char)])))

;; TODO:
;; Create a highlighter stream of spans to apply syntax highlighting
;; to a document. It _probably_ is not performant in the slightest bit, but it could help.
;; See syntax.rs and highlight event + Styles. Might be possible to map ansi code -> style,
;; and then access enough of the document API to make it possible.

(skip-compile
 (define *ansi-parser* (make-ansi-tokenizer))
 ;; This is a bit silly, but we'll have to model cursor movement.
 (define *cursor-position* 1)
 (define (helix-clear-line)

   (helix.static.extend_to_line_bounds)
   (helix.static.delete_selection))
 (define escape-code-map
   ;; EraseToEndOfLine - helix.static.kill_to_line_end
   (list (lambda (cx)
           (when (equal? 1 *cursor-position*)
             (helix-clear-line)))
         ;; EraseToStartOfLine
         helix.static.kill_to_line_start
         ;; EraseLine
         (lambda ()
           (helix.static.extend_to_line_bounds)
           (helix.static.delete_selection))
         ;; Cursor Position escape sequence
         (lambda () (helix.static.insert_string "CURSOR POSITION ESCAPE SEQUENCE"))))
 (define (async-write-from-terminal-loop line)

   (unless (hash-try-get *temporary-buffer-map* "steel-repl")
     (open-repl))

   (temporarily-switch-focus
    (lambda ()
      ;; Open up the repl, write the output
      (open-labelled-buffer "steel-repl")

      (transduce (tokenize-line *ansi-parser* line)
                 (into-for-each (lambda (line)
                                  (cond
                                    ; (write-line-to-terminal cx (to-string "ESCAPE CODE:" line))
                                    [(int? line) ((list-ref escape-code-map line) cx)]
                                    [(char? line) (write-char-to-terminal cx line)]
                                    ; (write-line-to-terminal cx line)
                                    [line (helix.static.insert_string cx line)]
                                    [else void])))))))
 (define (write-from-terminal-loop)

   (unless (hash-try-get *temporary-buffer-map* "steel-repl")
     (open-repl))

   (temporarily-switch-focus (lambda ()
                               ;; Open up the repl, write the output
                               (let ([output (async-try-read-line *pty-process*)])
                                 (if output
                                     (begin
                                       (open-labelled-buffer "steel-repl")
                                       (helix.static.insert_string output)
                                       (read-until-no-more-lines))
                                     (mark-failed!))))))
 ;; Every time we send a command, we can just unpark the delay
 (provide send-ls)
 (define (send-ls)
   (reset-terminal-refresh-delay!)
   (reset-fail-check!)
   (pty-process-send-command *pty-process* "ls -l\r"))
 ; (require "steel/transducers/transducers.scm")
 (provide send-command)
 (define (send-command . args)

   (define carriage-return-ammended-string
     (list-transduce (tadd-between " ") rcons (append args '("\r"))))

   (pty-process-send-command *pty-process* (apply string-append carriage-return-ammended-string))))

(define-syntax skip-compile
  (syntax-rules ()
    [(skip-compile) (begin)]
    [(skip-compile expr) (begin)]
    [(skip-compile expr exprs ...)
     (begin
       (skip-compile exprs ...))]))
