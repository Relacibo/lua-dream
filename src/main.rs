use clap::Parser as ClapParser;
use lua_dream_lexer::Lexer;
use lua_dream_parser::Parser;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;

#[derive(ClapParser, Debug)]
#[command(author, version, about = "Lua Dream Interpreter")]
struct Args {
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let Args { input } = &args;

    let mut br: Box<dyn BufRead> = match &input {
        Some(path) => {
            if path.to_str() == Some("-") {
                // TODO: Maybe make own error
                panic!("Path not supported");
            }
            let file = File::open(path)?;
            Box::new(BufReader::new(file))
        }
        _ => Box::new(BufReader::new(io::stdin().lock())),
    };

    // TODO: Maybe make own error
    let tokens = Lexer::new(input.as_ref().map(|p| p.as_path()), &mut br)
        .tokenize()
        .unwrap();

    // dbg!(&tokens);
    let ast = Parser::new(&tokens).parse_block().unwrap();
    dbg!(&ast);
    Ok(())
}
