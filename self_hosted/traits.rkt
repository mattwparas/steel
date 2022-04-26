;; When calling a trait method, resolve by identifying the type
(define *v-table* (hash))

;; Resolve type name -> method
;; 
(make-struct Trait (name functions))