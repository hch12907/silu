mod lexer;
#[macro_use] mod parser;
//mod preprocessor;
mod utils;

fn main() {
    let cont = include_str!("main.c");

    let _lex = lexer::Lexer::new(
        concat!(
            "+-* /<>()[]{},.;&|^~! \n",
            "+=-=*=/=<=>=++--<<>>&=|=^=~=!=// \n",
            " >>=<<=/// \"hello\" 0x1234567890ABCDEF \n",
            "-01234567 +1234567890L 0b10101010 123.456+789.0F\n",
            "struct abc {}; return a;\n",
            "++= /* */"
        )
    );

    let lex = lexer::Lexer::new(cont);
    let mut parser = parser::Parser::new(lex);

    println!("{:#?}", parser.parse_declaration());

    /*let expr = match parser.parse_expression() {
        parser::ParseResult::Parsed(_s, p) => p,
        parser::ParseResult::Error(s, e) => panic!("{:?} {:?}", s, e),
        parser::ParseResult::Unhandled => panic!("Found unhandled token"),
    };*/

    //print(bin, 1);
    //println!("{:#?}", parser::Ast::from_expression(expr));
}
