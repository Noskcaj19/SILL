fn main() {
   let data = include_str!("../sample.sill");
   match syntax::tokenize(data) {
      Ok(tokens) => println!("{:#?}", tokens),
      Err((err, tokens)) => {
         println!("{:?}\n{:#?}", err, tokens);
         match err {
            syntax::token::SpannedError(_, span) => {
               if let Some(span) = span {
                  println!("{}", &data[span.start as usize..span.end as usize])
               }
            }
         }
      }
   }
}
