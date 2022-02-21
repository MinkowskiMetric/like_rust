use like_rust::Lexer;

fn main() {
    let _tokens: Vec<_> = Lexer::from("a b c d e").collect();
}
