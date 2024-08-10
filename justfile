
# run unit tests on the compiler and virtual machine.
test:
    cargo test --all

# watch changes on the compiler
watch-compiler args: (watch args "gattic")

# watch changes in the current directory, clear the term and run the specified
# binary
watch args who:
    cargo watch --no-vcs-ignores -s "clear && cargo run --bin={{who}} -- {{args}}"
