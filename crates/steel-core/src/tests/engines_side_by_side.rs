use crate::steel_vm::engine::Engine;

const COMES_AND_GOES: &str = include_str!("success/threads_come_and_go.scm");

#[test]
fn engines_whose_threads_come_and_go() {
    let engines: Vec<_> = (0..6)
        .map(|_| {
            std::thread::spawn(|| {
                let mut engine = Engine::new();
                engine
                    .compile_and_run_raw_program(COMES_AND_GOES)
                    .map(|_| ())
                    .map_err(|e| e.to_string())
            })
        })
        .collect();

    for engine in engines {
        engine
            .join()
            .expect("the engine's thread finishes")
            .expect("the program runs");
    }
}
