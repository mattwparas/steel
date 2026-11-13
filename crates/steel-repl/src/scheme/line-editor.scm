;; steel/line-editor: reads one input, which may span several lines, with
;; emacs style editing. Built on #%private/steel/terminal; all terminal
;; access goes through a terminal-io, so tests can swap in a fake one.
;;
;; Keys are chars for text, or strings naming them: "C-a", "M-f", "up",
;; "C-left", "S-tab", "enter", "backspace", "escape" and so on. A keymap
;; maps keys to commands, procedures that take the edit state.

(require-builtin #%private/steel/terminal)

(provide make-line-editor
         line-editor-read-line!
         line-editor-keymap
         line-editor-history
         line-editor-completer
         set-line-editor-completer!
         line-editor-highlighter
         set-line-editor-highlighter!
         identifier-completer
         steel-highlighter
         current-line-editor
         make-terminal-io
         default-keymap
         keymap-set!
         keymap-ref
         keymap-remove!
         history-entries
         history-add!
         ;; For writing commands
         edit-text
         edit-cursor
         edit-insert!
         edit-set-text!
         edit-set-cursor!
         edit-accept!)

(define esc (string (integer->char 27)))
(define (csi . parts)
  (apply string-append esc "[" parts))

;;;; Terminal access

;; read-byte takes a timeout in ms, or #f to wait, and returns a byte, #f
;; on timeout, or eof. size returns (columns rows) or #f.
(struct terminal-io (read-byte write size raw-on raw-off))

(define (make-terminal-io read-byte write size raw-on raw-off)
  (terminal-io read-byte write size raw-on raw-off))

(define real-terminal
  (terminal-io read-byte/timeout
               (lambda (text)
                 (display text)
                 (flush-output-port (current-output-port)))
               terminal-size
               enable-raw-mode!
               disable-raw-mode!))

;;;; Keys

;; How long the rest of an escape sequence may take to arrive. A lone ESC
;; is the escape key.
(define escape-timeout-ms 30)

(define (read-key io)
  (define byte ((terminal-io-read-byte io) #f))
  (if (eof-object? byte)
      byte
      (decode-byte io byte)))

(define (decode-byte io byte)
  (cond
    [(= byte 27) (decode-escape io)]
    [(= byte 13) "enter"]
    [(= byte 10) "C-j"]
    [(= byte 9) "tab"]
    [(= byte 127) "backspace"]
    [(= byte 8) "C-h"]
    [(= byte 0) "C-space"]
    [(< byte 27) (string-append "C-" (string (integer->char (+ byte 96))))]
    [(< byte 32) (string-append "C-" (string (integer->char (+ byte 64))))]
    [(< byte 128) (integer->char byte)]
    [else (decode-utf8 io byte)]))

(define (alt key)
  (if (char? key)
      (string-append "M-" (string key))
      (string-append "M-" key)))

(define (decode-escape io)
  (define next ((terminal-io-read-byte io) escape-timeout-ms))
  (cond
    [(not (integer? next)) "escape"]
    [(= next 91) (decode-csi io)]
    [(= next 79) (decode-ss3 io)]
    [(= next 27) "M-escape"]
    [else (alt (decode-byte io next))]))

;; ESC [ parameters final, the final byte being in @ through ~
(define (decode-csi io)
  (let loop ([bytes '()])
    (define byte ((terminal-io-read-byte io) escape-timeout-ms))
    (cond
      [(not (integer? byte)) "unknown"]
      [(and (>= byte 64) (<= byte 126))
       (csi-key (list->string (map integer->char (reverse bytes))) (integer->char byte))]
      [(> (length bytes) 16) "unknown"]
      [else (loop (cons byte bytes))])))

;; xterm's modifier parameter: 1 + shift + 2 alt + 4 ctrl
(define (modifier-prefix parameter)
  (define bits (- parameter 1))
  (string-append (if (odd? (quotient bits 4)) "C-" "")
                 (if (odd? (quotient bits 2)) "M-" "")
                 (if (odd? bits) "S-" "")))

(define (csi-key parameters final)
  (define fields (split-many parameters ";"))
  (define (field i)
    (if (> (length fields) i)
        (string->number (list-ref fields i))
        #f))
  (define modifier (or (field 1) 1))
  (define base
    (cond
      [(equal? final #\A) "up"]
      [(equal? final #\B) "down"]
      [(equal? final #\C) "right"]
      [(equal? final #\D) "left"]
      [(equal? final #\H) "home"]
      [(equal? final #\F) "end"]
      [(equal? final #\Z) "S-tab"]
      [(equal? final #\~)
       (let ([code (field 0)])
         (cond
           [(member code '(1 7)) "home"]
           [(member code '(4 8)) "end"]
           [(equal? code 2) "insert"]
           [(equal? code 3) "delete"]
           [(equal? code 5) "page-up"]
           [(equal? code 6) "page-down"]
           [(equal? code 200) "paste-start"]
           [(equal? code 201) "paste-end"]
           [else #f]))]
      [else #f]))
  (if base
      (string-append (modifier-prefix modifier) base)
      "unknown"))

;; ESC O x, which some terminals send for arrows and F1-F4
(define (decode-ss3 io)
  (define byte ((terminal-io-read-byte io) escape-timeout-ms))
  (if (integer? byte)
      (let ([final (integer->char byte)])
        (cond
          [(equal? final #\A) "up"]
          [(equal? final #\B) "down"]
          [(equal? final #\C) "right"]
          [(equal? final #\D) "left"]
          [(equal? final #\H) "home"]
          [(equal? final #\F) "end"]
          [(equal? final #\P) "f1"]
          [(equal? final #\Q) "f2"]
          [(equal? final #\R) "f3"]
          [(equal? final #\S) "f4"]
          [else "unknown"]))
      "M-O"))

(define (decode-utf8 io lead)
  (define count
    (cond
      [(>= lead 240) 3]
      [(>= lead 224) 2]
      [(>= lead 192) 1]
      [else 0]))
  (let loop ([bytes (list lead)] [left count])
    (if (= left 0)
        (with-handler (lambda (_) "unknown")
                      (string-ref (bytes->string/utf8 (list->bytes (reverse bytes))) 0))
        (let ([byte ((terminal-io-read-byte io) 100)])
          (if (integer? byte)
              (loop (cons byte bytes) (- left 1))
              "unknown")))))

;;;; Keymaps

(struct keymap (bindings) #:mutable)

(define (keymap-set! km key command)
  (set-keymap-bindings! km (hash-insert (keymap-bindings km) key command))
  void)

(define (keymap-ref km key)
  (hash-try-get (keymap-bindings km) key))

(define (keymap-remove! km key)
  (set-keymap-bindings! km (hash-remove (keymap-bindings km) key))
  void)

;;;; History

;; entries are newest first. Saved in rustyline's format, so both editors
;; share the file: a #V2 line, then one entry per line with \n and \\
;; escaped.
(struct history (entries path needs-header) #:mutable)

(define history-limit 1000)

(define (escape-history line)
  (let loop ([chars (string->list line)] [out '()])
    (cond
      [(null? chars) (list->string (reverse out))]
      [(equal? (car chars) #\newline) (loop (cdr chars) (cons #\n (cons #\\ out)))]
      [(equal? (car chars) #\\) (loop (cdr chars) (cons #\\ (cons #\\ out)))]
      [else (loop (cdr chars) (cons (car chars) out))])))

(define (unescape-history line)
  (let loop ([chars (string->list line)] [out '()])
    (cond
      [(null? chars) (list->string (reverse out))]
      [(and (equal? (car chars) #\\) (pair? (cdr chars)))
       (loop (cddr chars) (cons (if (equal? (cadr chars) #\n) #\newline (cadr chars)) out))]
      [else (loop (cdr chars) (cons (car chars) out))])))

(define (take-at-most items count)
  (if (or (null? items) (= count 0))
      '()
      (cons (car items) (take-at-most (cdr items) (- count 1)))))

(define (read-history-file path)
  (define text
    (if (and path (path-exists? path))
        (with-handler (lambda (_) "") (read-port-to-string (open-input-file path)))
        ""))
  (define lines (filter (lambda (line) (not (equal? line ""))) (split-many text "\n")))
  (define v2 (and (pair? lines) (equal? (car lines) "#V2")))
  (define entries
    (if v2
        (map unescape-history (cdr lines))
        lines))
  (history (take-at-most (reverse entries) history-limit) path (equal? text "")))

(define (history-add! history line)
  (define entries (history-entries history))
  (unless (or (equal? line "")
              (and (pair? entries) (equal? (car entries) line)))
    (set-history-entries! history (take-at-most (cons line entries) history-limit))
    (when (history-path history)
      (append-history-file! history line))))

;; Appending only, so a REPL running alongside loses nothing. Failing to
;; write history doesn't stop editing.
(define (append-history-file! history line)
  (with-handler (lambda (_) #f)
                (let ([port (open-output-file (history-path history) #:exists 'append)])
                  (when (history-needs-header history)
                    (write-string "#V2\n" port)
                    (set-history-needs-header! history #f))
                  (write-string (string-append (escape-history line) "\n") port)
                  (close-output-port port))))

;;;; Editors

;; complete? decides whether Enter ends the input. completer and
;; highlighter are #f or procedures, see Completion and Highlighting.
(struct line-editor (io keymap history kill-ring complete? completer highlighter) #:mutable)

(define *current-line-editor* #f)

;; The editor the REPL is reading with, to change its keymap from the REPL
(define (current-line-editor)
  *current-line-editor*)

(define (make-line-editor [history-path #f] [io real-terminal])
  (define editor
    (line-editor io (default-keymap) (read-history-file history-path) '() input-complete? #f #f))
  (set! *current-line-editor* editor)
  editor)

;;;; Edit state, for one read

(struct edit
        (editor io
                prompt
                prompt-width
                text
                cursor
                ;; Row the cursor was left on by the last draw, counted from the
                ;; input's first row
                cursor-row
                ;; Whether the last draw ended by moving onto a fresh row
                wrapped-end
                ;; Which entry is showing while going through history, and what
                ;; was being typed before
                history-index
                saved-text
                this-command
                last-command
                ;; (start . end) of the text the last yank put in
                yank-span
                ;; While Tab cycles: (start typed candidates index), index -1
                ;; meaning what was typed
                completion
                ;; Candidates shown under the input, (candidates . selected)
                menu
                result)
        #:mutable)

(define (edit-insert! state text)
  (define current (edit-text state))
  (define at (edit-cursor state))
  (set-edit-text! state
                  (string-append (substring current 0 at)
                                 text
                                 (substring current at (string-length current))))
  (set-edit-cursor! state (+ at (string-length text))))

(define (edit-set-text! state text)
  (set-edit-text! state text)
  (set-edit-cursor! state (string-length text)))

;; Clamped to the text
(define (edit-set-cursor! state at)
  (set-edit-cursor! state (max 0 (min at (string-length (edit-text state))))))

(define (edit-accept! state)
  (set-edit-result! state (edit-text state)))

;; Removes [from, to) and returns it
(define (delete-range! state from to)
  (define current (edit-text state))
  (define removed (substring current from to))
  (set-edit-text! state
                  (string-append (substring current 0 from)
                                 (substring current to (string-length current))))
  (set-edit-cursor! state from)
  removed)

;;;; Moving by graphemes, words and lines

;; Char offsets where graphemes start, and the end
(define (boundaries text)
  (let loop ([graphemes (string-graphemes text)] [at 0] [found (list 0)])
    (if (null? graphemes)
        (reverse found)
        (let ([next (+ at (string-length (car graphemes)))])
          (loop (cdr graphemes) next (cons next found))))))

(define (previous-boundary text at)
  (let loop ([offsets (boundaries text)] [best 0])
    (if (or (null? offsets) (>= (car offsets) at))
        best
        (loop (cdr offsets) (car offsets)))))

(define (next-boundary text at)
  (let loop ([offsets (boundaries text)])
    (cond
      [(null? offsets) (string-length text)]
      [(> (car offsets) at) (car offsets)]
      [else (loop (cdr offsets))])))

(define (char-at text at)
  (string-ref text at))

(define (word-char? char)
  (not (or (char-whitespace? char) (member char '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\,)))))

;; Past any non-word chars, then past the word
(define (forward-word-end text at)
  (define end (string-length text))
  (let skip ([at at])
    (if (and (< at end) (not (word-char? (char-at text at))))
        (skip (+ at 1))
        (let word ([at at])
          (if (and (< at end) (word-char? (char-at text at)))
              (word (+ at 1))
              at)))))

(define (backward-word-start text at)
  (let skip ([at at])
    (if (and (> at 0) (not (word-char? (char-at text (- at 1)))))
        (skip (- at 1))
        (let word ([at at])
          (if (and (> at 0) (word-char? (char-at text (- at 1))))
              (word (- at 1))
              at)))))

;; Back to whitespace, like a shell's C-w
(define (backward-space-start text at)
  (let skip ([at at])
    (if (and (> at 0) (char-whitespace? (char-at text (- at 1))))
        (skip (- at 1))
        (let word ([at at])
          (if (and (> at 0) (not (char-whitespace? (char-at text (- at 1)))))
              (word (- at 1))
              at)))))

(define (line-start text at)
  (if (and (> at 0) (not (equal? (char-at text (- at 1)) #\newline)))
      (line-start text (- at 1))
      at))

(define (line-end text at)
  (if (and (< at (string-length text)) (not (equal? (char-at text at) #\newline)))
      (line-end text (+ at 1))
      at))

;;;; Commands

(define (beginning-of-line state)
  (set-edit-cursor! state (line-start (edit-text state) (edit-cursor state))))

(define (end-of-line state)
  (set-edit-cursor! state (line-end (edit-text state) (edit-cursor state))))

(define (beginning-of-input state)
  (set-edit-cursor! state 0))

(define (end-of-input state)
  (set-edit-cursor! state (string-length (edit-text state))))

(define (backward-char state)
  (set-edit-cursor! state (previous-boundary (edit-text state) (edit-cursor state))))

(define (forward-char state)
  (set-edit-cursor! state (next-boundary (edit-text state) (edit-cursor state))))

(define (backward-word state)
  (set-edit-cursor! state (backward-word-start (edit-text state) (edit-cursor state))))

(define (forward-word state)
  (set-edit-cursor! state (forward-word-end (edit-text state) (edit-cursor state))))

(define (delete-char state)
  (define at (edit-cursor state))
  (when (< at (string-length (edit-text state)))
    (delete-range! state at (next-boundary (edit-text state) at))))

(define (delete-char-or-eof state)
  (if (equal? (edit-text state) "")
      (set-edit-result! state 'eof)
      (delete-char state)))

(define (backward-delete-char state)
  (define at (edit-cursor state))
  (when (> at 0)
    (delete-range! state (previous-boundary (edit-text state) at) at)))

;; Kills in a row build up one kill ring entry: forward kills add to its
;; end, backward kills to its start
(define (kill-text! state from to backward?)
  (when (< from to)
    (let* ([editor (edit-editor state)]
           [removed (delete-range! state from to)]
           [ring (line-editor-kill-ring editor)])
      (set-line-editor-kill-ring!
       editor
       (if (and (equal? (edit-last-command state) 'kill) (pair? ring))
           (cons (if backward?
                     (string-append removed (car ring))
                     (string-append (car ring) removed))
                 (cdr ring))
           (take-at-most (cons removed ring) 30)))))
  (set-edit-this-command! state 'kill))

(define (kill! state from to)
  (kill-text! state from to #f))

(define (kill-backward! state from to)
  (kill-text! state from to #t))

;; To the end of the line, or the line break when already there
(define (kill-line state)
  (define text (edit-text state))
  (define at (edit-cursor state))
  (define end (line-end text at))
  (kill! state at (if (and (= end at) (< at (string-length text))) (+ at 1) end)))

(define (backward-kill-line state)
  (define at (edit-cursor state))
  (kill-backward! state (line-start (edit-text state) at) at))

(define (kill-word state)
  (define at (edit-cursor state))
  (kill! state at (forward-word-end (edit-text state) at)))

(define (backward-kill-word state)
  (define at (edit-cursor state))
  (kill-backward! state (backward-word-start (edit-text state) at) at))

(define (unix-word-rubout state)
  (define at (edit-cursor state))
  (kill-backward! state (backward-space-start (edit-text state) at) at))

(define (yank state)
  (define ring (line-editor-kill-ring (edit-editor state)))
  (define start (edit-cursor state))
  (when (pair? ring)
    (edit-insert! state (car ring))
    (set-edit-yank-span! state (cons start (edit-cursor state)))
    (set-edit-this-command! state 'yank)))

;; Right after a yank, swaps what it put in for the next kill
(define (yank-pop state)
  (define editor (edit-editor state))
  (define ring (line-editor-kill-ring editor))
  (when (and (equal? (edit-last-command state) 'yank) (pair? ring))
    (let ([span (edit-yank-span state)]
          [rotated (append (cdr ring) (list (car ring)))])
      (set-line-editor-kill-ring! editor rotated)
      (delete-range! state (car span) (cdr span))
      (edit-insert! state (car rotated))
      (set-edit-yank-span! state (cons (car span) (edit-cursor state)))
      (set-edit-this-command! state 'yank))))

(define (transpose-chars state)
  (define text (edit-text state))
  (define at (edit-cursor state))
  (define size (string-length text))
  (when (and (> at 0) (>= size 2))
    ;; At the end, swap the last two
    (let* ([middle (if (= at size) (previous-boundary text at) at)]
           [start (previous-boundary text middle)]
           [end (next-boundary text middle)])
      (when (< start middle)
        (let ([before (substring text start middle)]
              [after (substring text middle end)])
          (delete-range! state start end)
          (edit-insert! state (string-append after before)))))))

(define (show-history-entry state index)
  (define entries (history-entries (line-editor-history (edit-editor state))))
  (unless (edit-history-index state)
    (set-edit-saved-text! state (edit-text state)))
  (set-edit-history-index! state index)
  (edit-set-text! state (if index (list-ref entries index) (edit-saved-text state))))

(define (history-previous state)
  (define entries (history-entries (line-editor-history (edit-editor state))))
  (define index (edit-history-index state))
  (define next (if index (+ index 1) 0))
  (when (< next (length entries))
    (show-history-entry state next)))

(define (history-next state)
  (define index (edit-history-index state))
  (when index
    (show-history-entry state (if (= index 0) #f (- index 1)))))

;; Same column on the line above or below, as far as it goes
(define (move-line state direction)
  (define text (edit-text state))
  (define at (edit-cursor state))
  (define start (line-start text at))
  (define column (- at start))
  (define target-start
    (if (< direction 0)
        (line-start text (- start 1))
        (+ (line-end text at) 1)))
  (set-edit-cursor! state (min (+ target-start column) (line-end text target-start))))

(define (previous-line-or-history state)
  (if (> (line-start (edit-text state) (edit-cursor state)) 0)
      (move-line state -1)
      (history-previous state)))

(define (next-line-or-history state)
  (define text (edit-text state))
  (if (< (line-end text (edit-cursor state)) (string-length text))
      (move-line state 1)
      (history-next state)))

;; Enter: done if the input is a whole form, otherwise a new line
(define (accept-line state)
  (if ((line-editor-complete? (edit-editor state)) (edit-text state))
      (edit-accept! state)
      (edit-insert! state "\n")))

(define (insert-newline state)
  (edit-insert! state "\n"))

(define (interrupt state)
  (set-edit-result! state
                    (if (equal? (edit-text state) "")
                        'interrupted-empty
                        'interrupted)))

(define (clear-screen state)
  ((terminal-io-write (edit-io state)) (string-append (csi "H") (csi "2J")))
  (set-edit-cursor-row! state 0))

;; Tab where there's nothing to complete
(define (indent state)
  (edit-insert! state "  "))

(define (default-keymap)
  (define km (keymap (hash)))
  (for-each (lambda (binding) (keymap-set! km (car binding) (cadr binding)))
            (list (list "C-a" beginning-of-line)
                  (list "home" beginning-of-line)
                  (list "C-e" end-of-line)
                  (list "end" end-of-line)
                  (list "M-<" beginning-of-input)
                  (list "M->" end-of-input)
                  (list "C-b" backward-char)
                  (list "left" backward-char)
                  (list "C-f" forward-char)
                  (list "right" forward-char)
                  (list "M-b" backward-word)
                  (list "C-left" backward-word)
                  (list "M-f" forward-word)
                  (list "C-right" forward-word)
                  (list "C-d" delete-char-or-eof)
                  (list "delete" delete-char)
                  (list "backspace" backward-delete-char)
                  (list "C-h" backward-delete-char)
                  (list "C-k" kill-line)
                  (list "C-u" backward-kill-line)
                  (list "M-d" kill-word)
                  (list "M-backspace" backward-kill-word)
                  (list "C-w" unix-word-rubout)
                  (list "C-y" yank)
                  (list "M-y" yank-pop)
                  (list "C-t" transpose-chars)
                  (list "up" previous-line-or-history)
                  (list "C-p" previous-line-or-history)
                  (list "down" next-line-or-history)
                  (list "C-n" next-line-or-history)
                  (list "enter" accept-line)
                  (list "C-j" insert-newline)
                  (list "M-enter" insert-newline)
                  (list "C-c" interrupt)
                  (list "C-l" clear-screen)
                  (list "tab" complete)
                  (list "C-r" reverse-search)
                  (list "paste-start" paste)))
  km)

(define (key-name key)
  (if (char? key)
      (string key)
      key))

;; A command that raises shouldn't end the read: show the error under the
;; input, then carry on drawing below it
(define (report-command-error! state key err)
  (define cursor (edit-cursor state))
  (set-edit-cursor! state (string-length (edit-text state)))
  (render! state)
  ((terminal-io-write (edit-io state))
   (string-append (if (edit-wrapped-end state) "" "\n")
                  "The command for "
                  (key-name key)
                  " failed: "
                  (to-string err)
                  "\n"))
  (set-edit-cursor! state (min cursor (string-length (edit-text state))))
  (set-edit-cursor-row! state 0))

(define (run-key! state key)
  (define command (keymap-ref (line-editor-keymap (edit-editor state)) key))
  (set-edit-this-command! state #f)
  (cond
    [command (with-handler (lambda (err) (report-command-error! state key err)) (command state))]
    [(char? key) (edit-insert! state (string key))]
    ;; Unbound, nothing to do
    [else #f])
  ;; Anything but another Tab settles the completion
  (unless (equal? (edit-this-command state) 'complete)
    (set-edit-completion! state #f)
    (set-edit-menu! state #f))
  (set-edit-last-command! state (edit-this-command state)))

;;;; Completion

;; A completer takes the text and the cursor, and returns #f when there's
;; nothing to complete there, or (start end candidates): the span to
;; replace and what could go there, best first.

(define (replace-span! state start end text)
  (delete-range! state start end)
  (edit-insert! state text))

(define (common-prefix strings)
  (define first-string (car strings))
  (let loop ([length 0])
    (if (and (< length (string-length first-string))
             (all-have? strings (+ length 1) (substring first-string 0 (+ length 1))))
        (loop (+ length 1))
        (substring first-string 0 length))))

(define (all-have? strings length prefix)
  (cond
    [(null? strings) #t]
    [(and (>= (string-length (car strings)) length) (starts-with? (car strings) prefix))
     (all-have? (cdr strings) length prefix)]
    [else #f]))

;; One candidate goes straight in. Several: in goes what they all start
;; with, they're listed under the input, and more Tabs go through them,
;; then back to what was typed.
(define (complete state)
  (define previous (edit-completion state))
  (define completer (line-editor-completer (edit-editor state)))
  (cond
    [(and previous (equal? (edit-last-command state) 'complete)) (cycle-completion! state previous)]
    [(not completer) (indent state)]
    [else
     (let ([found (completer (edit-text state) (edit-cursor state))])
       (if found
           (complete-span! state (list-ref found 0) (list-ref found 1) (list-ref found 2))
           (indent state)))]))

(define (complete-span! state start end candidates)
  (cond
    [(null? candidates) #f]
    [(null? (cdr candidates)) (replace-span! state start end (car candidates))]
    [else
     (let* ([typed (substring (edit-text state) start end)]
            ;; Of the ones that start with what was typed: the others only
            ;; contain it
            [extending (filter (lambda (candidate) (starts-with? candidate typed)) candidates)]
            [prefix (if (null? extending) typed (common-prefix extending))]
            [replacement (if (and (> (string-length prefix) (string-length typed))
                                  (starts-with? prefix typed))
                             prefix
                             typed)])
       (replace-span! state start end replacement)
       (set-edit-completion! state (list start replacement candidates -1))
       (set-edit-menu! state (cons candidates -1))
       (set-edit-this-command! state 'complete))]))

(define (cycle-completion! state completion)
  (define start (list-ref completion 0))
  (define typed (list-ref completion 1))
  (define candidates (list-ref completion 2))
  (define next
    (let ([index (+ (list-ref completion 3) 1)])
      (if (>= index (length candidates)) -1 index)))
  (replace-span! state start (edit-cursor state) (if (= next -1) typed (list-ref candidates next)))
  (set-edit-completion! state (list start typed candidates next))
  (set-edit-menu! state (cons candidates next))
  (set-edit-this-command! state 'complete))

(define (find-first matches? items)
  (cond
    [(null? items) #f]
    [(matches? (car items)) (car items)]
    [else (find-first matches? (cdr items))]))

;; Builtins and #% names after the rest
(define (rank<? a b)
  (define (weight name)
    (+ (if (string-contains? name "builtin") 2 0) (if (starts-with? name "#") 1 0)))
  (define weight-a (weight a))
  (define weight-b (weight b))
  (if (= weight-a weight-b)
      (string<? a b)
      (< weight-a weight-b)))

;; Internal names (#%prim. aliases, mangled module names) only when asked
;; for with a #
(define (offered? name typed)
  (or (not (starts-with? name "#")) (starts-with? typed "#")))

;; Completes the identifier under the cursor from `(names)`: names starting
;; with it first, then names containing it
(define (identifier-completer names)
  (lambda (text cursor)
    (define token
      (find-first (lambda (token)
                    (and (equal? (car token) 'identifier)
                         (<= (cadr token) cursor)
                         (<= cursor (caddr token))))
                  (tokenize text)))
    (if token
        (let* ([start (cadr token)]
               [end (caddr token)]
               [typed (substring text start end)]
               [all (filter (lambda (name) (offered? name typed)) (names))])
          (list start
                end
                (append (sort (filter (lambda (name) (starts-with? name typed)) all) rank<?)
                        (sort (filter (lambda (name)
                                        (and (not (starts-with? name typed))
                                             (string-contains? name typed)))
                                      all)
                              rank<?))))
        #f)))

;;;; Highlighting

;; A highlighter takes the text and the cursor and returns the text with
;; SGR escapes added; nothing else may change.

(define (sgr code)
  (string-append esc "[" code "m"))

(define (brackets-match? open close)
  (member (string open close) '("()" "[]" "{}")))

;; Where the bracket paired with the one the cursor touches is: a closer
;; just before the cursor, else a bracket at it, else an opener just before
(define (bracket-partner tokens text cursor)
  (define pairs
    (let loop ([tokens tokens] [stack '()] [pairs '()])
      (cond
        [(null? tokens) pairs]
        [(equal? (caar tokens) 'open) (loop (cdr tokens) (cons (cadar tokens) stack) pairs)]
        [(equal? (caar tokens) 'close)
         (let ([close (cadar tokens)])
           (if (and (pair? stack) (brackets-match? (string-ref text (car stack)) (string-ref text close)))
               (loop (cdr tokens) (cdr stack) (cons (cons (car stack) close) pairs))
               (loop (cdr tokens) (if (pair? stack) (cdr stack) stack) pairs)))]
        [else (loop (cdr tokens) stack pairs)])))
  (define (bracket? kind at)
    (find-first (lambda (token) (and (equal? (car token) kind) (= (cadr token) at))) tokens))
  (define touching
    (cond
      [(and (> cursor 0) (bracket? 'close (- cursor 1))) (- cursor 1)]
      [(or (bracket? 'open cursor) (bracket? 'close cursor)) cursor]
      [(and (> cursor 0) (bracket? 'open (- cursor 1))) (- cursor 1)]
      [else #f]))
  (define pair
    (and touching
         (find-first (lambda (pair) (or (= (car pair) touching) (= (cdr pair) touching))) pairs)))
  (cond
    [(not pair) #f]
    [(= (car pair) touching) (cdr pair)]
    [else (car pair)]))

(define (paint text spans)
  (let loop ([spans spans] [at 0] [out '()])
    (if (null? spans)
        (apply string-append (reverse (cons (substring text at (string-length text)) out)))
        (let* ([span (car spans)]
               [start (car span)]
               [end (cadr span)])
          (if (< start at)
              (loop (cdr spans) at out)
              (loop (cdr spans)
                    end
                    (cons (string-append (sgr (caddr span)) (substring text start end) (sgr "0"))
                          (cons (substring text at start) out))))))))

;; Steel syntax colours, and the bracket matching the one at the cursor in
;; bold blue. `global?` says which identifiers are bound.
(define (steel-highlighter [global? (lambda (name) #f)])
  (lambda (text cursor)
    (define tokens (tokenize text))
    (define partner (bracket-partner tokens text cursor))
    (define colored
      (filter (lambda (span) span)
              (map (lambda (token)
                     (define kind (car token))
                     (define start (cadr token))
                     (define end (caddr token))
                     (define code
                       (cond
                         [(member kind '(form boolean)) "95"]
                         [(equal? kind 'number) "93"]
                         [(equal? kind 'string) "92"]
                         [(and (equal? kind 'identifier) (global? (substring text start end))) "94"]
                         [else #f]))
                     (and code (list start end code)))
                   tokens)))
    (paint text
           (sort (if partner
                     (cons (list partner (+ partner 1) "1;34") colored)
                     colored)
                 (lambda (a b) (< (car a) (car b)))))))

;;;; Pasting

;; ESC [ 2 0 1 ~, the end of a paste, backwards
(define paste-end '(126 49 48 50 91 27))

(define (ends-with-paste-end? bytes-backwards)
  (let loop ([bytes bytes-backwards] [marker paste-end])
    (cond
      [(null? marker) #t]
      [(null? bytes) #f]
      [(= (car bytes) (car marker)) (loop (cdr bytes) (cdr marker))]
      [else #f])))

;; Line endings to \n, tabs to two spaces, other control characters dropped
(define (clean-paste text)
  (let loop ([chars (string->list text)] [out '()])
    (cond
      [(null? chars) (list->string (reverse out))]
      [(equal? (car chars) #\return)
       (if (and (pair? (cdr chars)) (equal? (cadr chars) #\newline))
           (loop (cdr chars) out)
           (loop (cdr chars) (cons #\newline out)))]
      [(equal? (car chars) #\tab) (loop (cdr chars) (cons #\space (cons #\space out)))]
      [(equal? (car chars) #\newline) (loop (cdr chars) (cons #\newline out))]
      [(< (char->integer (car chars)) 32) (loop (cdr chars) out)]
      [else (loop (cdr chars) (cons (car chars) out))])))

(define (decode-paste bytes)
  (with-handler (lambda (_)
                  (list->string (map (lambda (byte) (if (< byte 128) (integer->char byte) #\?)) bytes)))
                (bytes->string/utf8 (list->bytes bytes))))

;; Everything up to the end marker goes in as is: no keys, so a newline in a
;; paste doesn't end the input
(define (paste state)
  (define read (terminal-io-read-byte (edit-io state)))
  (let loop ([bytes '()])
    (define byte (read #f))
    (cond
      [(eof-object? byte) (edit-insert! state (clean-paste (decode-paste (reverse bytes))))]
      [(ends-with-paste-end? (cons byte bytes))
       (edit-insert! state (clean-paste (decode-paste (reverse (list-tail (cons byte bytes) 6)))))]
      [else (loop (cons byte bytes))])))

;;;; History search

(define (string-search text query)
  (define size (string-length text))
  (define wanted (string-length query))
  (let loop ([at 0])
    (cond
      [(> (+ at wanted) size) #f]
      [(equal? (substring text at (+ at wanted)) query) at]
      [else (loop (+ at 1))])))

;; The first entry from `index` on (older) containing `query`, as
;; (index . position)
(define (history-search entries query index)
  (let loop ([entries (if (< index (length entries)) (list-tail entries index) '())] [index index])
    (cond
      [(null? entries) #f]
      [(string-search (car entries) query) (cons index (string-search (car entries) query))]
      [else (loop (cdr entries) (+ index 1))])))

;; C-r: type to search history, newest first. C-r again for the next older
;; match, C-g or escape to go back to what was there, any other key to keep
;; the match and carry on with that key.
(define (reverse-search state)
  (define io (edit-io state))
  (define entries (history-entries (line-editor-history (edit-editor state))))
  (define prompt (edit-prompt state))
  (define prompt-width (edit-prompt-width state))
  (define original-text (edit-text state))
  (define original-cursor (edit-cursor state))
  (define (show! query found failed)
    (define label
      (string-append (if failed "(failed reverse-i-search)`" "(reverse-i-search)`") query "': "))
    (set-edit-prompt! state label)
    (set-edit-prompt-width! state (string-display-width label))
    (when found
      (set-edit-text! state (list-ref entries (car found)))
      (set-edit-cursor! state (cdr found)))
    (render! state))
  (define (leave!)
    (set-edit-prompt! state prompt)
    (set-edit-prompt-width! state prompt-width))
  (show! "" #f #f)
  (let loop ([query ""] [found #f])
    (define key (read-key io))
    (cond
      [(eof-object? key) (leave!)]
      [(member key '("C-g" "escape"))
       (set-edit-text! state original-text)
       (set-edit-cursor! state original-cursor)
       (leave!)]
      [(equal? key "C-r")
       (let ([next (and found (history-search entries query (+ (car found) 1)))])
         (show! query (or next found) (and found (not next)))
         (loop query (or next found)))]
      [(member key '("backspace" "C-h"))
       (let* ([shorter (if (equal? query "") "" (substring query 0 (- (string-length query) 1)))]
              [next (if (equal? shorter "") #f (history-search entries shorter 0))])
         (show! shorter next (and (not (equal? shorter "")) (not next)))
         (loop shorter next))]
      [(char? key)
       (let* ([longer (string-append query (string key))]
              [next (history-search entries longer (if found (car found) 0))])
         (show! longer (or next found) (not next))
         (loop longer (or next found)))]
      [else
       (leave!)
       (run-key! state key)])))

;;;; Drawing

(define (strip-escapes text)
  (define escape (integer->char 27))
  (let loop ([chars (string->list text)] [out '()])
    (cond
      [(null? chars) (list->string (reverse out))]
      [(and (equal? (car chars) escape) (pair? (cdr chars)) (equal? (cadr chars) #\[))
       (let skip ([rest (cddr chars)])
         (cond
           [(null? rest) (list->string (reverse out))]
           [(let ([code (char->integer (car rest))]) (and (>= code 64) (<= code 126)))
            (loop (cdr rest) out)]
           [else (skip (cdr rest))]))]
      [(equal? (car chars) escape) (loop (cdr chars) out)]
      [else (loop (cdr chars) (cons (car chars) out))])))

(define (terminal-columns io)
  (define size ((terminal-io-size io)))
  (if (and size (> (car size) 0))
      (car size)
      80))

;; Walks the input the way the terminal will lay it out, wrapping at
;; `columns` and starting each line after the first under the prompt.
;; Returns (cursor-row cursor-column end-row end-column): the cursor as
;; (row column), and where writing the input leaves the terminal. That can
;; be one past the last column, the terminal waiting to wrap.
(define (layout text prompt-width columns cursor)
  (let loop ([graphemes (string-graphemes text)]
             [at 0]
             [row (quotient prompt-width columns)]
             [column (remainder prompt-width columns)]
             [cursor-at #f])
    (define here
      (if (>= column columns)
          (list (+ row 1) 0)
          (list row column)))
    (define found (or cursor-at (and (= at cursor) here)))
    (if (null? graphemes)
        (append (or found here) (list row column))
        (let* ([grapheme (car graphemes)]
               [next (+ at (string-length grapheme))])
          (if (equal? grapheme "\n")
              (loop (cdr graphemes) next (+ row 1) (remainder prompt-width columns) found)
              (let ([width (string-display-width grapheme)])
                (if (> (+ column width) columns)
                    (loop (cdr graphemes) next (+ row 1) width found)
                    (loop (cdr graphemes) next row (+ column width) found))))))))

(define (continued text width)
  (define pad (make-string width #\space))
  (let loop ([chars (string->list text)] [out '()])
    (cond
      [(null? chars) (list->string (reverse out))]
      [(equal? (car chars) #\newline)
       (loop (cdr chars) (append (reverse (string->list pad)) (cons #\newline out)))]
      [else (loop (cdr chars) (cons (car chars) out))])))

;; At most this many rows of completion candidates
(define menu-max-rows 8)

(define (pad-right text width)
  (string-append text (make-string (max 0 (- width (string-display-width text))) #\space)))

;; Cut down to `width` columns, with an ellipsis when cut
(define (fit text width)
  (if (<= (string-display-width text) width)
      text
      (let loop ([graphemes (string-graphemes text)] [used 0] [out '()])
        (define grapheme-width (if (null? graphemes) 0 (string-display-width (car graphemes))))
        (if (or (null? graphemes) (> (+ used grapheme-width) (- width 1)))
            (string-append (apply string-append (reverse out)) "…")
            (loop (cdr graphemes) (+ used grapheme-width) (cons (car graphemes) out))))))

(define (take-rows items per-row)
  (if (null? items)
      '()
      (cons (take-at-most items per-row)
            (take-rows (if (> (length items) per-row) (list-tail items per-row) '()) per-row))))

;; The candidates in columns, the page holding the selected one. Rows stay
;; narrower than the terminal, so they never wrap.
(define (menu-rows candidates selected columns)
  (define limit (max 1 (- columns 1)))
  (define cell (min limit (+ 2 (apply max (map string-display-width candidates)))))
  (define per-row (max 1 (quotient limit cell)))
  (define total (length candidates))
  (define page-size
    (if (> total (* per-row menu-max-rows))
        (* per-row (- menu-max-rows 1))
        total))
  (define first-shown (* page-size (quotient (max selected 0) page-size)))
  (define shown (take-at-most (list-tail candidates first-shown) page-size))
  (define (cell-text candidate index)
    (define text (fit candidate (- cell 2)))
    (string-append (if (= index selected) (string-append (sgr "7") text (sgr "0")) text)
                   (make-string (- cell (string-display-width text)) #\space)))
  (define rows
    (let loop ([row-items (take-rows shown per-row)] [index first-shown] [rows '()])
      (if (null? row-items)
          (reverse rows)
          (loop (cdr row-items)
                (+ index (length (car row-items)))
                (cons (let cells ([items (car row-items)] [index index] [out '()])
                        (if (null? items)
                            (apply string-append (reverse out))
                            (cells (cdr items) (+ index 1) (cons (cell-text (car items) index) out))))
                      rows)))))
  (define rest (- total (+ first-shown (length shown))))
  (if (> rest 0)
      (append rows (list (fit (string-append "… and " (number->string rest) " more") limit)))
      rows))

(define (highlighted state)
  (define highlighter (line-editor-highlighter (edit-editor state)))
  (if highlighter
      ;; A broken highlighter shouldn't stop editing
      ;; Once the read is over there's no cursor to match a bracket at, so
      ;; what stays on the screen has no bold bracket
      (with-handler (lambda (_) (edit-text state))
                    (highlighter (edit-text state) (if (edit-result state) -1 (edit-cursor state))))
      (edit-text state)))

;; Redraws the prompt and input from the input's first row, and any
;; completion candidates under it, and puts the cursor back
(define (render! state)
  (define io (edit-io state))
  (define columns (terminal-columns io))
  (define width (edit-prompt-width state))
  (define spot (layout (edit-text state) width columns (edit-cursor state)))
  (define cursor-row (list-ref spot 0))
  (define cursor-column (list-ref spot 1))
  (define end-row (list-ref spot 2))
  (define up (edit-cursor-row state))
  (define menu (edit-menu state))
  (define menu-lines
    (if menu
        (menu-rows (car menu) (cdr menu) columns)
        '()))
  (define (back-to-cursor from-row)
    (string-append (if (> from-row cursor-row) (csi (number->string (- from-row cursor-row)) "A") "")
                   "\r"
                   (if (> cursor-column 0) (csi (number->string cursor-column) "C") "")))
  (define after
    (cond
      ;; The menu starts on a new line whether or not the input ended
      ;; waiting to wrap
      [(pair? menu-lines)
       (string-append "\n"
                      (string-join menu-lines "\n")
                      (back-to-cursor (+ end-row (length menu-lines))))]
      ;; Past the last column at the very end: a new line wraps for real
      [(> cursor-row end-row) "\n"]
      [else (back-to-cursor end-row)]))
  ((terminal-io-write io)
   (string-append (if (> up 0) (csi (number->string up) "A") "")
                  "\r"
                  (csi "J")
                  (edit-prompt state)
                  (continued (highlighted state) width)
                  after))
  (set-edit-cursor-row! state cursor-row)
  (set-edit-wrapped-end! state (and (null? menu-lines) (> cursor-row end-row))))

;; Leaves the cursor on the line after the input
(define (finish! state)
  (set-edit-cursor! state (string-length (edit-text state)))
  (set-edit-menu! state #f)
  (render! state)
  (unless (edit-wrapped-end state)
    ((terminal-io-write (edit-io state)) "\n")))

;;;; Reading

;; A string, 'eof, or 'interrupted / 'interrupted-empty for C-c with and
;; without anything typed
(define (line-editor-read-line! editor prompt)
  (define io (line-editor-io editor))
  (define state
    (edit editor
          io
          prompt
          (string-display-width (strip-escapes prompt))
          ""
          0
          0
          #f
          #f
          ""
          #f
          #f
          #f
          #f
          #f
          #f))
  ((terminal-io-raw-on io))
  ;; Pastes come wrapped in ESC[200~ ... ESC[201~
  ((terminal-io-write io) (csi "?2004h"))
  (define result
    (with-handler (lambda (err)
                    ((terminal-io-write io) (csi "?2004l"))
                    ((terminal-io-raw-off io))
                    (raise-error err))
                  (render! state)
                  (let loop ()
                    (define key (read-key io))
                    (if (eof-object? key)
                        (set-edit-result! state
                                          (if (equal? (edit-text state) "")
                                              'eof
                                              (edit-text state)))
                        (run-key! state key))
                    (if (edit-result state)
                        (edit-result state)
                        (begin
                          (render! state)
                          (loop))))))
  (finish! state)
  ((terminal-io-write io) (csi "?2004l"))
  ((terminal-io-raw-off io))
  (when (string? result)
    (history-add! (line-editor-history editor) result))
  result)
