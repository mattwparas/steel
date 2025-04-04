use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args = std::env::args()
        .skip(1)
        .skip_while(|x| x == "steel-lib")
        .collect();
    cargo_steel_lib::run(args, Vec::new())
}
