use std::{env, error::Error, fs::read_to_string, path::PathBuf};

use gatti::{
    errors::{DiagCtxt, PartialResult},
    lexer::Lexer,
    VERSION_AND_GIT_HASH,
};
use termcolor::{ColorChoice, StandardStream};

fn main() -> Result<(), Box<dyn Error>> {
    let help = format!(
        "\
{} {}
{}
Compiler for the Gatti programming language.

USAGE:
    gic [FLAGS] <file>

ARGS:
    <file>    The file to compile into bytecode.

FLAGS: (todo)
    -h, --help           Prints help information
    --color {{always|auto|never}}
                         Specify color printing mode
                         - always: Always print in color
                         - auto: Automatically decide based on the environment
                           and the terminal.
                         - never: Never print in colors
    -V, --version        Prints version information
",
        env!("CARGO_BIN_NAME"),
        VERSION_AND_GIT_HASH,
        env!("CARGO_PKG_AUTHORS"),
    );

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print!("{}", help);
        std::process::exit(1);
    }

    println!(" /\\_/\\  Gatti\n( o.o )\n");

    let path = PathBuf::from(&args[1]);
    let buf = read_to_string(&path)?;
    let mut s = StandardStream::stdout(ColorChoice::Auto);

    let dcx = DiagCtxt::new(&buf, &path);

    let mut lexer = Lexer::new(&path, &buf, &dcx);
    let res = lexer.lex();

    match res {
        PartialResult::Good(toks) => {
            dbg!(toks);
        }
        PartialResult::Fuzzy(toks, dgs) => {
            dbg!(toks);
            dcx.emit_diags(dgs)
        }
        PartialResult::Fail(dgs) => dcx.emit_diags(dgs),
    }

    dcx.render_all(&mut s);
    Ok(())
}
