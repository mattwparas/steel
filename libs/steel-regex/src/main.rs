fn main() {
    steel_regex::build_module()
        .emit_package_to_file("libsteel_regex", "regex.scm")
        .unwrap()
}
