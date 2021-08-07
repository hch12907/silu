use crate::lexer::Keyword;
use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Labeled(Box<LabeledStmt>),
    Compound(Box<CompoundStatement>),
    Expr(Option<Expression>),
    Selection(Box<SelectionStmt>),
    Iteration(Box<IterationStmt>),
    Jump(Box<JumpStmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement(Vec<DeclOrStmt>);

#[derive(Clone, Debug, PartialEq)]
pub enum DeclOrStmt {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LabeledStmt {
    Label(Identifier, Statement),
    Case(Expression, Statement),
    Default(Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SelectionStmt {
    If(Expression, Statement, Option<Statement>),
    Switch(Expression, Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IterationStmt {
    While(Expression, Statement),
    DoWhile(Statement, Expression),
    For(Option<InitStmt>, Option<Expression>, Option<Expression>, Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitStmt {
    Expr(Expression),
    Decl(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub enum JumpStmt {
    Goto(Identifier),
    Continue,
    Break,
    Return(Option<Expression>),
}

impl<T: TokenStreamer> Parser<T> {
    pub(super) fn parse_statement(&mut self) -> ParseResult<Statement> {
        macro_rules! statement_helper {
            ($stmt:ident, $stmt_func:ident) => {
                let parsed = try_parse_opt!(self.$stmt_func());
        
                if let Some((span, stmt)) = parsed {
                    return ParseResult::Parsed(span, Statement::$stmt(Box::new(stmt)))
                }
            };
        }

        statement_helper!(Labeled, parse_labeled_statement);
        statement_helper!(Compound, parse_compound_statement);
        statement_helper!(Selection, parse_selection_statement);
        statement_helper!(Iteration, parse_iteration_statement);
        statement_helper!(Jump, parse_jump_statement);

        if let Some((span, expr)) = try_parse_opt!(self.parse_expression()) {
            expect_token!(self, Token::Semicolon);
            let expr = Expression::from_comma_expression(expr);
            return ParseResult::Parsed(span, Statement::Expr(Some(expr)))
        };

        if let Some(span) = expect_token_opt!(self, Token::Semicolon).span() {
            return ParseResult::Parsed(span, Statement::Expr(None))
        };

        ParseResult::Unhandled
    }

    pub(super) fn parse_compound_statement(&mut self) -> ParseResult<CompoundStatement> {
        match self.current_scope {
            Scope::Function { ref mut scope_depth, .. } => *scope_depth += 1,
            Scope::File { .. } => panic!("unexpected file scope"),
        };

        let result = if let Some(span) = expect_token_opt!(self, Token::BraceOpen).span() {
            let mut stmts = Vec::new();
            while let ParseResult::Parsed(_, p) = self.parse_declaration_or_statement() {
                stmts.push(p);
            }
            expect_token!(self, Token::BraceClose);
            Some((span, CompoundStatement(stmts)))
        } else {
            None
        };

        match self.current_scope {
            Scope::Function { ref mut scope_depth, .. } => *scope_depth -= 1,
            Scope::File { .. } => panic!("unexpected file scope"),
        };

        self.symbol_table.leave_scope(&self.current_scope);

        match result {
            Some((span, stmt)) => ParseResult::Parsed(span, stmt),
            None => ParseResult::Unhandled,
        }
    }

    pub(super) fn parse_declaration_or_statement(&mut self) -> ParseResult<DeclOrStmt> {
        let decl = try_parse_opt!(self.parse_declaration());
        if let Some((span, decl)) = decl {
            ParseResult::Parsed(span, DeclOrStmt::Declaration(decl))
        } else {
            let stmt = try_parse_opt!(self.parse_statement());
            
            if let Some((span, stmt)) = stmt {
                ParseResult::Parsed(span, DeclOrStmt::Statement(stmt))
            } else {
                ParseResult::Unhandled
            }
        }
    }

    pub(super) fn parse_labeled_statement(&mut self) -> ParseResult<LabeledStmt> {
        let (span, result) = if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Case)).span() {
            let expr = try_parse!(self.parse_conditional_expression()).1;
            let expr = Expression::from_comma_expression(wrap!(conditional expr));
            expect_token!(self, Token::Colon);
            let stmt = try_parse!(self.parse_statement()).1;
            (span, LabeledStmt::Case(expr, stmt))
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Default)).span() {
            expect_token!(self, Token::Colon);
            let stmt = try_parse!(self.parse_statement()).1;
            (span, LabeledStmt::Default(stmt))
        } else {
            if matches!(self.peek_nth(0), Some((_, Token::Ident(_)))) {
                if matches!(self.peek_nth(1), Some((_, Token::Colon))) {
                    let (span, id) = self.eat().unwrap();
                    let id = force_unwrap!(id, Token::Ident(i) => i);
                    self.eat().unwrap();
                    let stmt = try_parse!(self.parse_statement()).1;
                    (span, LabeledStmt::Label(Identifier(id), stmt))
                } else {
                    return ParseResult::Unhandled
                }
            } else {
                return ParseResult::Unhandled
            }
        };

        ParseResult::Parsed(span, result)
    }

    pub(super) fn parse_selection_statement(&mut self) -> ParseResult<SelectionStmt> {
        let (span, result) = if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::If)).span() {
            expect_token!(self, Token::ParenOpen);
            let expr = try_parse!(self.parse_expression()).1;
            let expr = Expression::from_comma_expression(expr);
            expect_token!(self, Token::ParenClose);
            let then_stmt = try_parse!(self.parse_statement()).1;

            let else_stmt = if expect_token_opt!(self, Token::Keyword(Keyword::Else)).is_obtained() {
                Some(try_parse!(self.parse_statement()).1)
            } else {
                None
            };

            (span, SelectionStmt::If(expr, then_stmt, else_stmt))
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Switch)).span() {
            expect_token!(self, Token::ParenOpen);
            let expr = try_parse!(self.parse_expression()).1;
            let expr = Expression::from_comma_expression(expr);
            expect_token!(self, Token::ParenClose);
            let stmt = try_parse!(self.parse_statement()).1;
            (span, SelectionStmt::Switch(expr, stmt))
        } else {
            return ParseResult::Unhandled
        };

        ParseResult::Parsed(span, result)
    }

    pub(super) fn parse_iteration_statement(&mut self) -> ParseResult<IterationStmt> {
        let (span, result) = if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::While)).span() {
            expect_token!(self, Token::ParenOpen);
            let expr = try_parse!(self.parse_expression()).1;
            let expr = Expression::from_comma_expression(expr);
            expect_token!(self, Token::ParenClose);
            let stmt = try_parse!(self.parse_statement()).1;
            (span, IterationStmt::While(expr, stmt))
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Do)).span() {
            let stmt = try_parse!(self.parse_statement()).1;
            expect_token!(self, Token::Keyword(Keyword::Do));
            expect_token!(self, Token::ParenOpen);
            let expr = try_parse!(self.parse_expression()).1;
            let expr = Expression::from_comma_expression(expr);
            expect_token!(self, Token::ParenClose);
            expect_token!(self, Token::Semicolon);
            (span, IterationStmt::DoWhile(stmt, expr))
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::For)).span() {
            expect_token!(self, Token::ParenOpen);
            let init = try_parse_opt!(self.parse_expression())
                .map(|(_, e)| InitStmt::Expr(Expression::from_comma_expression(e)));

            let init = if init.is_none() {
                try_parse_opt!(self.parse_declaration())
                    .map(|(_, d)| InitStmt::Decl(d))
            } else {
                expect_token!(self, Token::Semicolon);
                init
            };

            let cond = try_parse_opt!(self.parse_expression())
                .map(|(_, e)| Expression::from_comma_expression(e));
            expect_token!(self, Token::Semicolon);

            let after = try_parse_opt!(self.parse_expression())
                .map(|(_, e)| Expression::from_comma_expression(e));
            expect_token!(self, Token::ParenClose);

            let stmt = try_parse!(self.parse_statement()).1;
            (span, IterationStmt::For(init, cond, after, stmt))
        } else {
            return ParseResult::Unhandled
        };

        ParseResult::Parsed(span, result)
    }

    pub(super) fn parse_jump_statement(&mut self) -> ParseResult<JumpStmt> {
        let (span, result) = if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Goto)).span() {
            let (_, tok) = expect_token!(self, Token::Ident(_));
            let id = match tok {
                Token::Ident(id) => Identifier(id),
                _ => unreachable!(),
            };
            (span, JumpStmt::Goto(id))
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Continue)).span() {
            (span, JumpStmt::Continue)
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Break)).span() {
            (span, JumpStmt::Break)
        } else if let Some(span) = expect_token_opt!(self, Token::Keyword(Keyword::Return)).span() {
            let expr = try_parse_opt!(self.parse_expression());
            let expr = expr.map(|(_s, e)| Expression::from_comma_expression(e));
            (span, JumpStmt::Return(expr))
        } else {
            return ParseResult::Unhandled
        };

        expect_token!(self, Token::Semicolon);
        ParseResult::Parsed(span, result)
    }
}
