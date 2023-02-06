use crate::steel_vm::register_fn::RegisterFn;
use crate::{rvals::Custom, steel_vm::builtin::BuiltInModule};

use colored::{ColoredString, Colorize};

impl Custom for ColoredString {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{}", self)))
    }
}

macro_rules! wrap_coloring {
    ($($name:ident),* $(,)?) => {

        $ (
            fn $name(string: String) -> ColoredString {
                string.$name()
            }
        ) *

        pub fn string_coloring_module() -> BuiltInModule {
            let mut module = BuiltInModule::new("steel/strings/colors".to_string());

            $ (
                module.register_fn(stringify!($name), $name);
            ) *

            module

        }
    };
}

wrap_coloring! {
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    purple,
    cyan,
    white,
    bright_black,
    bright_red,
    bright_green,
    bright_yellow,
    bright_blue,
    bright_white,
    on_black,
    on_red,
    on_green,
    on_yellow,
    on_blue,
    on_magenta,
    on_purple,
    on_cyan,
    on_white,
    on_bright_black,
    on_bright_red,
    on_bright_green,
    on_bright_yellow,
    on_bright_blue,
    on_bright_magenta,
    on_bright_purple,
    on_bright_cyan,
    on_bright_white,
    normal,
    bold,
    dimmed,
    italic,
    underline,
    blink,
    reversed,
    hidden,
    strikethrough
}
