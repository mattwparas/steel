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
                          code-block-indented?
                          code-block-fenced?
                          code-block->fenced
                          event->html-string
                          event->string))

(define Softbreak 'SoftBreak)
(define HardBreak 'HardBreak)
(define Rule 'Rule)

(define *zygote* (Engine::new))

(define (eval expr)
  (define engine (Engine::clone *zygote*))
  (run! engine expr))

(define my-parser (parser "```scheme,run-in-playground
(list 10 20 30)
(list 20 30 40 50)
```"))

; (displayln (~> (parser-next my-parser) event->start-tag tag->code-block ))
; (displayln (map eval (split-many (event->text (parser-next my-parser)) "\n")))
; (displayln (~> (parser-next my-parser) event->end-tag tag->code-block))
