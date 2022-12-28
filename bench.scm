(->
    (command "hyperfine" '("target/release/steel steel_examples/fib.rkt" "python3 scratch/fib1.py" "--warmup" "10" "--min-runs" "40"))
    (spawn-process)
    (Ok->value)
    (wait))