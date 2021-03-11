use steel_vm::engine::Engine;

pub fn main() {
    let mut vm = Engine::new();

    vm.on_progress(|count| {
        // parameter is 'u64' - number of operations already performed
        if count % 1000 == 0 {
            println!("Number of instructions up to this point: {}", count); // print out a progress log every 1,000 operations

            // Returning false here would quit the evaluation of the function
            return true;
        }
        true
    });

    // This should end with "Number of instructions up to this point: 12000"
    vm.run(
        r#"
        (define (loop x)
            (if (equal? x 1000)
                x
                (loop (+ x 1))))
        (loop 0)
    "#,
    )
    .unwrap();
}
