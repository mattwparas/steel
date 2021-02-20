use super::tokens::TokenType;

enum Expr {
    Atom(TokenType),
    List(Vec<Expr>),
}
