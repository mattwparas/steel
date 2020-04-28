/*
be able to parse macros
steps:
1. Identify macro instance
    (defmacro any-number-of-args)
2. Construct and store macro struct inside the evaluation environment
3. Provide function to expand syntax according to the rules contained within macro struct


(define-syntax while
    (syntax-rules (do)
        [(while cond do body ...)
            (let loop ()
                (when cond
                    body ...
                    (loop)))]))
(while (> x 0) do
    (displayln x)
    (set! x (- x 1)))

*/

// #[macro_export]
// macro_rules! mixed_rules {
//     () => {};
//     (trace $name:ident; $($tail:tt)*) => {
//         {
//             println!(concat!(stringify!($name), " = {:?}"), $name);
//             mixed_rules!($($tail)*);
//         }
//     };
//     (trace $name:ident = $init:expr; $($tail:tt)*) => {
//         {
//             let $name = $init;
//             println!(concat!(stringify!($name), " = {:?}"), $name);
//             mixed_rules!($($tail)*);
//         }
//     };
// }

// macro_rules! and {
//     ($arg1:expr, $args2:expr) => {
//         format!("(if {} (if {} #f) #f)", $arg1, $arg2)
//     };

//     ($($args:expr),*) => {
//         format!()
//     };
// }

#[macro_export]
macro_rules! and {
    () => {};
    ($name:tt) => {
            format!("(if {} #t #f)", stringify!($name))
    };
    ($name:tt $($tail:tt)*) => {
            format!("(if {} {} #f)", stringify!($name), and!($($tail)*))
    };
}

#[macro_export]
macro_rules! or {
    () => {};
    ($name:tt) => {
            format!("(if {} #t #f)", stringify!($name))
    };
    ($name:tt $($tail:tt)*) => {
            format!("(if {} #t {})", stringify!($name), and!($($tail)*))
    };
}

/*
(and a b) => (if a (if b #t #f) #f)
*/
