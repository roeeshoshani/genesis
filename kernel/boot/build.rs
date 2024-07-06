use std::env::current_dir;

fn main() {
    let linker_script = current_dir().unwrap().join("boot.lds");
    println!("cargo::rerun-if-changed={}", linker_script.display());
    println!("cargo::rustc-link-arg=-T{}", linker_script.display());
}
