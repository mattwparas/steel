(define (build-release)
    (-> (command "cargo" '("build" "--release"))
        (spawn-process)
        (Ok->value)
        (wait)))

(define (run-bench args)
    (-> (command "hyperfine" args)
        (spawn-process)
        (Ok->value)
        (wait)))


(define (main)
    (displayln "Building steel for release...")
    (build-release)
    (displayln "Running benches...")
    (run-bench '("../target/release/steel fib/fib.scm" "python3 fib/fib.py" "lua fib/fib.lua" "--warmup" "10" "--min-runs" "40"))
    (run-bench '("../target/release/steel ack/ack.scm" "python3 ack/ack.py" "lua ack/ack.lua" "--warmup" "10" "--min-runs" "40"))
    (run-bench '("../target/release/steel bin-trees/bin-trees.scm" "python3 bin-trees/bin_trees.py" "--warmup" "5"))
    (displayln "Done"))

(main)