mod helpers;
use helpers::*;

#[test]
fn basic_test() {
    test_from_files("input_tests.rkt", "output_tests.rkt");
}
