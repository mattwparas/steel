use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;
use crate::steel_vm::register_fn::RegisterFn;

fn host_engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_fn("host-sum-bytes", |value: SteelVal| -> isize {
        match value {
            SteelVal::ByteVector(bytes) => {
                bytes.with_bytes(|bytes| bytes.iter().map(|&x| x as isize).sum())
            }
            _ => -1,
        }
    });
    engine.register_fn("host-double-bytes!", |value: SteelVal| {
        if let SteelVal::ByteVector(bytes) = value {
            bytes.with_bytes_mut(|bytes| {
                for byte in bytes {
                    *byte = byte.wrapping_mul(2);
                }
            })
        }
    });
    engine
}

#[test]
fn host_functions_read_and_write_the_bytes() {
    let mut engine = host_engine();
    engine
        .compile_and_run_raw_program(
            r#"
            (define b (bytes 1 2 3))
            (assert! (= (host-sum-bytes b) 6))
            (host-double-bytes! b)
            (assert! (equal? b (bytes 2 4 6)))

            ;; Compiled reads and writes of the same bytevector around the host's
            (define (fill! v i n)
              (when (< i n)
                (bytes-set! v i (modulo i 256))
                (fill! v (+ i 1) n)))
            (define (round v)
              (fill! v 0 (bytes-length v))
              (host-double-bytes! v)
              (+ (host-sum-bytes v) (bytes-ref v 3)))
            (define big (make-bytes 1000 0))
            (define (rounds k acc)
              (if (= k 0) acc (rounds (- k 1) (round big))))
            (assert! (= (rounds 200 0) (round big)))
            (assert! (= (bytes-ref big 3) 6))
            "#,
        )
        .unwrap();
}

#[test]
fn the_bytes_of_a_value_from_steel() {
    let mut engine = Engine::new();
    let values = engine
        .compile_and_run_raw_program("(bytes 10 20 30)")
        .unwrap();
    let Some(SteelVal::ByteVector(bytes)) = values.last() else {
        panic!("expected a bytevector, found {:?}", values.last());
    };

    assert_eq!(&*bytes.bytes(), &[10, 20, 30]);
    assert_eq!(bytes.len(), 3);
    assert_eq!(bytes.to_vec(), vec![10, 20, 30]);

    bytes.bytes_mut()[1] = 21;
    assert_eq!(bytes.to_vec(), vec![10, 21, 30]);

    // The copy the program still holds sees the change
    engine.register_value("from-host", SteelVal::ByteVector(bytes.clone()));
    engine
        .compile_and_run_raw_program("(assert! (equal? from-host (bytes 10 21 30)))")
        .unwrap();
}
