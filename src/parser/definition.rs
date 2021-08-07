use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub struct TranslationUnit(pub Vec<Definition>);

#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
    Func(Function),
    Var(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function(DeclarationSpecifier, Declarator, CompoundStatement);

impl<T: TokenStreamer> Parser<T> {
    pub fn parse_translation_unit(&mut self) -> ParseResult<TranslationUnit> {
        if self.peek().is_some() {
            let mut list = vec![try_parse!(self.parse_definition()).1];
            while let Some((_, p)) = try_parse_opt!(self.parse_definition()) {
                list.push(p);
            }
            ParseResult::Parsed(Span::default(), TranslationUnit(list))
        } else {
            match self.eat() {
                TokenResult::Obtained(span, tok) =>
                    ParseResult::Error(span, ParseErr::UnexpectedToken(tok)),
                TokenResult::EndOfStream(_) =>
                    ParseResult::Unhandled,
                TokenResult::Unwanted =>
                    unreachable!(),
            }
        }
    }

    fn parse_definition(&mut self) -> ParseResult<Definition> {
        let (span, decl) = try_parse!(self.parse_declaration());

        if let Some((_, Token::BraceOpen)) = self.peek() {
            let (spec, mut init) = force_unwrap!(decl, Declaration::Declare(spec, init) => (spec, init));

            if init.0.len() != 1 {
                return ParseResult::Error(span, ParseErr::InvalidFunctionSignature);
            }

            self.current_scope = Scope::Function {
                filename: force_unwrap!(&self.current_scope, Scope::File { filename } => filename.clone()),
                func_name: force_unwrap!(
                    { 
                        let init_decl = &init.0.first().unwrap();
                        let InitDeclarator(decl, _init) = init_decl;
                        let Declarator(_ptr, direct_decl) = decl;
                        direct_decl
                    },
                    DirectDeclarator::FuncPointer(ref decl, ref _params) =>
                        force_unwrap!(decl.as_ref(), DirectDeclarator::Ident(id) => id.clone())
                ),
                scope_depth: 0,
                shadowed: None,
            };

            let stmt = try_parse!(self.parse_compound_statement()).1;

            self.current_scope = Scope::File {
                filename: force_unwrap!(
                    &self.current_scope,
                    Scope::Function { filename, .. } => filename.clone()
                )
            };

            self.symbol_table.leave_scope(&self.current_scope);

            let InitDeclarator(decl, init) = init.0.pop().unwrap();

            if init.is_some() {
                return ParseResult::Error(span, ParseErr::InvalidFunctionSignature);
            }

            ParseResult::Parsed(span, Definition::Func(Function(spec, decl, stmt)))
        } else {
            ParseResult::Parsed(span, Definition::Var(decl))
        }
    }
}
