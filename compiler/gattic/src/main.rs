use std::{env, fs::read_to_string, path::PathBuf};

// use gatti::{interposer::Interposer, lexer::Lexer, parser::Parser, VERSION_AND_GIT_HASH};
use gattic_errors::{spans::VERSION_AND_GIT_HASH, DiagCtxt, PartialResult};
use gattic_interposer::Interposer;
use gattic_lexer::Lexer;
use gattic_parser::Parser;
use termcolor::{ColorChoice, StandardStream};

fn main() {
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

    // 0. Prepare for compilation, get the path and read the file
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print!("{}", help);
        std::process::exit(1);
    }

    println!(" /\\_/\\  Gatti\n( o.o )\n");

    let path = PathBuf::from(&args[1]);
    let buf = read_to_string(&path).expect("Couldn't read the file.");
    let mut s = StandardStream::stdout(ColorChoice::Auto);

    // 1. Create the diagnostic context
    let dcx = DiagCtxt::new(&buf, &path);

    // 2. Lex the source file to raw tokens
    let mut lexer = Lexer::new(&path, &buf, &dcx);
    let res = lexer.lex();

    let rtoks = match res {
        PartialResult::Good(toks) => toks,
        PartialResult::Fuzzy(toks, dgs) => {
            dcx.emit_diags(dgs);
            toks
        }
        PartialResult::Fail(dgs) => {
            dcx.emit_diags(dgs);
            dcx.render_all(&mut s);
            return;
        }
    };

    // 3. Interpose the token stream
    let mut interposer = Interposer::new(rtoks);
    let ts = interposer.run();

    println!("TOKEN STREAM = {ts}");

    // 4. Parse the token stream
    let mut parser = Parser::new(&dcx, ts);
    let parser_res = parser.run();

    let ast = match parser_res {
        PartialResult::Good(ast) => ast,
        PartialResult::Fuzzy(ast, dgs) => {
            dcx.emit_diags(dgs);
            ast
        }
        PartialResult::Fail(dgs) => {
            dcx.emit_diags(dgs);
            dcx.render_all(&mut s);
            return;
        }
    };
    dbg!(&ast);

    // X. Print the diagnostics (should only be warning, and not errors)
    dcx.render_all(&mut s);
}
