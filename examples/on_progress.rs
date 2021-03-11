use steel_vm::engine::Engine;

// It's possible to add a function that will get fun on every instruction call
// For instance, if you wanted to see how far you were getting in the evaluation of a program
// (perhaps you wanted to see that, idk) then you could add the closure using the
// `on_progress` method
//
// This is how the repl instruments the CTRL-C handler, by registering a closure that
// listens to the receiving end of a interrupt stream
pub fn main() {
    let mut vm = Engine::new();

    vm.on_progress(|count| {
        // parameter is 'usize' - number of instructions performed up to this point
        if count % 1000 == 0 {
            // print out a progress log every 1000 operations
            println!("Number of instructions up to this point: {}", count);
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
