(#%require-dylib "libsteel_markdown"
                 (only-in parse
                          parser
                          parser-next
                          event-text?
                          event-code?
                          event-html?
                          event-start?
                          event-end?
                          event->text
                          event->code
                          event->html
                          event->start-tag
                          event->end-tag
                          tag-paragraph?
                          tag-heading?
                          tag-block-quote?
                          tag-code-block?
                          tag-list?
                          tag-foot-note-definition?
                          tag-table?
                          tag-table-head?
                          tag-table-row?
                          tag-table-cell?
                          tag-emphasis?
                          tag-strong?
                          tag-strike-through?
                          tag-link?
                          tag-image?
                          tag->heading
                          tag->code-block
                          tag->end-code-block
                          code-block-indented?
                          code-block-fenced?
                          code-block->fenced
                          event->html-string
                          event->string
                          syntax-highlighter
                          syntax-highlighter/text->highlighted-html
                          set-event-code!
                          set-event-text!
                          set-event-as-html!))

(require "steel/fs/fs.scm")

(provide parse
         parser
         parser-next
         event-text?
         event-code?
         event-html?
         event-start?
         event-end?
         event->text
         event->code
         event->html
         event->start-tag
         event->end-tag
         tag-paragraph?
         tag-heading?
         tag-block-quote?
         tag-code-block?
         tag-list?
         tag-foot-note-definition?
         tag-table?
         tag-table-head?
         tag-table-row?
         tag-table-cell?
         tag-emphasis?
         tag-strong?
         tag-strike-through?
         tag-link?
         tag-image?
         tag->heading
         tag->code-block
         code-block-indented?
         code-block-fenced?
         code-block->fenced
         event->html-string
         event->string
         syntax-highlighter
         syntax-highlighter/text->highlighted-html
         set-event-code!
         set-event-text!
         set-event-as-html!
         mdfile->html-file
         mdfile->html-string
         event->start-code-block-label
         event->end-code-block-label
         mdfile->html-highlighted-string)

(define Softbreak 'SoftBreak)
(define HardBreak 'HardBreak)
(define Rule 'Rule)

;; Loads the markdown file located at the given path, and
;; renders it to an html string. Writes the html string out
;; to the given file.
(define (mdfile->html-file path dest)
  (define contents (file->string path))

  (let ([output (open-output-file dest)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output])
      (define next (parser-next mdparser))
      (when next
        (display (event->html-string next) sink)
        (loop mdparser sink)))))

;; Loads the markdown at the given path, and returns
;; it as an html string
(define (mdfile->html-string path)
  (define contents (file->string path))

  (let ([output (open-output-string)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output])
      (define next (parser-next mdparser))
      (when next
        (display (event->html-string next) sink)
        (loop mdparser sink)))

    (get-output-string output)))

(define (event->start-code-block-label event)
  (if (event-start? event)
      (begin
        (define event-start-tag (event->start-tag event))
        (define event-code-block (tag->code-block event-start-tag))

        (if (and event-code-block (code-block-fenced? event-code-block))
            (code-block->fenced event-code-block)
            #f))

      #f))

(define (event->end-code-block-label event)
  (if (event-end? event)
      (begin
        (define event-start-tag (event->end-tag event))
        (define event-code-block (tag->end-code-block event-start-tag))
        (if event-code-block #t #f))
      #f))

;; TODO: Add more languages, and make this mapping... make a bit more sense.
(define highlighter-map
  (hash "bash" "Bourne Again Shell (bash)" "scheme" "Lisp" "racket" "Lisp" "rust" "Rust"))

(define (mdfile->html-highlighted-string highlighter path)
  (define contents (file->string path))

  (let ([output (open-output-string)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output] [language #f])
      (define next (parser-next mdparser))
      ; (displayln language)
      (when next
        (define start-language (event->start-code-block-label next))
        (define end-language (event->end-code-block-label next))

        ;; If we have a language, then we use that to highlight the text
        ;; on the inner code block, and emit html there.
        (when (and language (event-text? next))
          ; (displayln "Highlighting language!" language)
          (set-event-as-html! next
                              (syntax-highlighter/text->highlighted-html highlighter
                                                                         (hash-ref highlighter-map
                                                                                   language)
                                                                         (event->text next))))

        ;; Write the event as an html string
        (display (event->html-string next) sink)
        (loop mdparser sink (if end-language #f (or start-language language)))))

    (get-output-string output)))

; mdfile->html-string "README.md")

; (displayln (~> (parser-next my-parser) event->start-tag tag->code-block ))
; (displayln (map eval (split-many (event->text (parser-next my-parser)) "\n")))
; (displayln (~> (parser-next my-parser) event->end-tag tag->code-block))
