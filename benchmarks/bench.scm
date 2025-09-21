(require "../cogs/srfi/srfi-28/format.scm")

(define print displayln)

(define (build-release)
  (~> (command "cargo" '("build" "--release")) (spawn-process) (Ok->value) (wait)))

(define (run-bench args)
  (~> (command "hyperfine" args) (spawn-process) (Ok->value) (wait)))

(define *interpreter-map*
  (hash "py"
        "python3.13"
        "scm"
		"../target/release/steel"
        ; "scm"
        ; "../target/aarch64-apple-darwin/release/steel"
        "lua"
        "lua"))

(define (extension->interpreter ext)
  (hash-get *interpreter-map* ext))

(define (combine-interpreter-and-path interpreter path)
  (string-append (string-append interpreter " ") path))

(define (path->command-fragment path)
  (define interpreter (~> path (path->extension) (extension->interpreter)))
  (if (list? interpreter)
      (map (lambda (interp) (combine-interpreter-and-path interp path)) interpreter)
      (combine-interpreter-and-path interpreter path)))

(define (directory->bench-command dir)
  (flatten (map path->command-fragment (read-dir dir))))

(define (bench-group dir . options)
  (run-bench (append (filter (lambda (x) (not (ends-with? x ".lua"))) (directory->bench-command dir))
                     options)))

(define *bench-hashmap*
  (hash 
	"Startup" '("startup" "--warmup" "10" "--min-runs" "100") 
	    "Map" '("map" "--warmup" "10")
        "Ack" '("ack" "--warmup" "10")
        "Fib" '("fib" "--warmup" "10" "--min-runs" "40")
        "Binary Trees" '("bin-trees" "--warmup" "10")
		))

(define *target-path* "../docs/src/benchmarks")

(define (export-md arg-list)
  (flatten (cons arg-list 
	(list "--export-markdown" 
	  (format "~a/~a.md" *target-path* (car arg-list))))))

;; surely we don't need to define this
(define (repeat val times)
  (map (λ (_) val) (range 0 times)))

(define (write-newlines num port)
  (#%raw-write-string (string-join (repeat "\n" num)) port))

;; TODO make sure the keys are ordered

(define (build-benchmark-doc bench-hashmap) 
  (define out (open-output-file (format "~a/benchmarks.md" *target-path*)))
  (#%raw-write-string "# Benchmarks" out)
  (write-newlines 2 out)
  (for-each (λ (key) (let ([name (car (hash-ref bench-hashmap key))])
	(#%raw-write-string (format "## ~a" key) out)
	(write-newlines 2 out)
	(#%raw-write-string (format "{{#include ~a.md}}" name) out)
	(write-newlines 2 out)))
	(hash-keys->list bench-hashmap))
  (close-port out))

(define (main)
  (print "Building steel for release...")
  (build-release)
  (print "Running benches...")
  (transduce (hash-values->list *bench-hashmap*)
             (mapping (lambda (args)
                        (newline)
                        (apply bench-group (export-md args)))) 
             (into-list))

  (build-benchmark-doc *bench-hashmap*)

  ; (bench-group "bin-trees")
  (print "Done"))

(main)

; (define (main)
;     (displayln "Building steel for release...")
;     (build-release)
;     (displayln "Running benches...")
;     (run-bench "../target/release/steel startup/startup.scm" "python3.10 startup/startup.py" "--warmup" "10" "--min-runs" "100")
;     (run-bench "../target/release/steel fib/fib.scm" "python3.10 fib/fib.py" "--warmup" "10" "--min-runs" "40")
;     ; (run-bench '("../target/release/steel fib/fib.scm" "python3 fib/fib.py" "lua fib/fib.lua" "--warmup" "10" "--min-runs" "40"))
;     ; (run-bench '("../target/release/steel ack/ack.scm" "python3 ack/ack.py" "lua ack/ack.lua" "--warmup" "10" "--min-runs" "40"))
;     ; (run-bench '("../target/release/steel bin-trees/bin-trees.scm" "python3 bin-trees/bin_trees.py" "--warmup" "5"))
;     (displayln "Done"))

; (main)
