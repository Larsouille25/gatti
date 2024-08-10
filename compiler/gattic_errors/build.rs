use std::{borrow::Cow, process::Command};

const MAJOR: &str = env!("CARGO_PKG_VERSION_MAJOR");
const MINOR: &str = env!("CARGO_PKG_VERSION_MINOR");
const PATCH: &str = env!("CARGO_PKG_VERSION_PATCH");

fn pretty_version() -> String {
    if PATCH == "0" {
        format!("{MAJOR}.{MINOR}")
    } else {
        format!("{MAJOR}.{MINOR}.{PATCH}")
    }
}

fn main() {
    let git_hash = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .filter(|output| output.status.success())
        .and_then(|x| String::from_utf8(x.stdout).ok());

    let version = pretty_version();
    let version_and_git_hash: Cow<_> = match git_hash {
        Some(hash) => format!("{version} ({})", &hash[..8]).into(),
        None => version.into(),
    };

    println!("cargo:rustc-env=VERSION_AND_GIT_HASH={version_and_git_hash}");
}
