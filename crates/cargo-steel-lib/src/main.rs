use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let args = std::env::args()
        .skip(1)
        .skip_while(|x| x == "steel-lib")
        .collect();
    if cargo_steel_lib::run(args, Vec::new())? {
        Ok(())
    } else {
        let err: Box<dyn Error> =
            String::from("cargo build failed to complete successfully").into();
        Err(err)
    }
}
