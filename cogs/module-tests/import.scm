(require (prefix-in export. (only-in "export.scm" thing-should-not-escape Applesauce bananas)))

export.Applesauce

export.bananas

(export.thing-should-not-escape 10)

;; Dead code analysis would be nice as well
;; If we can run constant evaluation over the result without actually
;; taking the const evaluation branches, we can store the
;; resulting removed spans and just render them in the LSP
(cond
  [(list? 10) (displayln "hello world!")]
  [else
   =>
   (displayln "foo bar")])
