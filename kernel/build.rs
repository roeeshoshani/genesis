fn main() {
    println!("cargo::rerun-if-changed=kernel.lds");
    println!("cargo::rustc-link-arg=-Tkernel.lds");
}
