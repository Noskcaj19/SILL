fn main() {
    let data = include_str!("../test.sill");
    let tokens = match syntax::tokenize(data) {
        Ok(tokens) => {
            println!("{:#?}", tokens);
            tokens
        }
        Err((err, tokens)) => {
            println!("{:?}\n{:#?}", err, tokens);
            match err {
                syntax::token::SpannedError(_, span) => {
                    if let Some(span) = span {
                        println!("{}", &data[span.start as usize..span.end as usize])
                    }
                }
            };
            return;
        }
    };

    // transform
    let mut new_tokens = Vec::new();
    for token in tokens {
        new_tokens.push(Ok((token.1.start as usize, token.0, token.1.end as usize)))
    }

    let mut errors = Vec::new();
    let parse =
        syntax::parse::grammar::ProgramParser::new().parse(&mut errors, new_tokens.into_iter());

    println!("{:#?}", parse);
}
