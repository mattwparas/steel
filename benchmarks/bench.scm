(define (run-bench args)
    (-> (command "hyperfine" args)
        (spawn-process)
        (Ok->value)
        (wait)))


(run-bench '("../target/release/steel steel/fib.scm" "python3 python/fib.py" "lua lua/fib.lua" "--warmup" "10" "--min-runs" "40"))
; (run-bench '("../target/release/steel steel/ack.scm" "python3 python/ack.py" "--warmup" "10" "--min-runs" "40"))
; (run-bench '("../target/release/steel steel/bin-trees.scm" "python3 python/bin_trees.py" "--warmup" "10" "--min-runs" "40"))
