; (define (build-list-chain x)
;   (if (equal? x 100000) (list x) (list x (build-list-chain (+ x 1)))))

; (define a (build-list-chain 0))

; (define (build-hashmap-chain x)
; (if (equal? x 100000) (hash 'a x) (hash 'a (build-hashmap-chain (+ x 1)))))

; (define a (build-hashmap-chain 0))

;; Testing deep collections
; (struct ConsCell (car cdr) #:mutable #:transparent)

; (define (build-list x)
;   (cond
;     [(equal? x 100000) void]
;     [else (ConsCell x (build-list (+ x 1)))]))

; (define (build-nested-list x y)
;   (cond
;     [(equal? x y) void]
;     [else (ConsCell x (build-nested-list (+ x 1) y))]))

; (let ([foo (build-nested-list 0 10000)]) #true)

; (provide create-file)

; (define (create-file cx)
;   (when (currently-in-labelled-buffer? cx FILE-TREE)
;     (define currently-selected (list-ref *file-tree* (helix.static.get-current-line-number cx)))
;     (define prompt
;       (if (is-dir? currently-selected)
;           (string-append "New file: " currently-selected "/")
;           (string-append "New file: "
;                          (trim-end-matches currently-selected (file-name currently-selected)))))

;     (helix-prompt!
;      cx
;      prompt
;      (lambda (cx result)
;        (define file-name (string-append (trim-start-matches prompt "New file: ") result))
;        (temporarily-switch-focus cx
;                                  (lambda (cx)
;                                    (helix.vsplit-new cx '() helix.PromptEvent::Validate)
;                                    (helix.open cx (list file-name) helix.PromptEvent::Validate)
;                                    (helix.write cx (list file-name) helix.PromptEvent::Validate)
;                                    (helix.quit cx '() helix.PromptEvent::Validate)))

;        ;; TODO:
;        ;; This is happening before the write is finished, so its not working. We will have to manually insert
;        ;; the new file into the right spot in the tree, which would require rewriting this to have a proper sorted
;        ;; tree representation in memory, which we don't yet have. For now, we can just do this I guess
;        (enqueue-thread-local-callback cx refresh-file-tree)))))
