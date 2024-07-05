use anyhow::Result;
use clap::Parser;
use devx_cmd::{cmd, run, Cmd};

#[derive(Parser)]
enum Cli {
    Build,
}
fn main() {
    if let Err(err) = run() {
        eprintln!("error: {:#?}", err);
    }
}

fn run() -> Result<()> {
    let cli = Cli::parse();
    match cli {
        Cli::Build => build()?,
    }
    Ok(())
}

fn build() -> Result<()> {
    cmd!("cargo", "build").current_dir("core").run()?;
    Ok(())
}
