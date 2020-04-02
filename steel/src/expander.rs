/*



be able to parse macros

steps:

1. Identify macro instance
    (defmacro any-number-of-args)



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
