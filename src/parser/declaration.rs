use crate::lexer::Keyword;
use crate::parser::*;
use TokenResult::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Declare(DeclarationSpecifier, InitDeclaratorList),
    StaticAssert(StaticAssertDeclaration),
}
#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationSpecifier(pub Vec<DeclarationSpecifierNode>);

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationSpecifierNode {
    Storage(StorageClass),
    TypeSpec(TypeSpecifier),
    TypeQualif(TypeQualifier),
    FuncSpec(FuncSpecifier)
}

#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclaratorList(pub Vec<InitDeclarator>);

#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclarator(pub Declarator, pub Option<Initializer>);

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClass {
    Auto,
    Extern,
    Register,
    Static,
    ThreadLocal,
    Typedef,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Struct(Option<Identifier>, StructDeclarationList),
    Union(Option<Identifier>, StructDeclarationList),
    Enum(Option<Identifier>, EnumeratorList),
    Typedef(Identifier),
    TypeName(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationList(pub Vec<StructDeclaration>);

#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclaration {
    Field(SpecifierQualifierList, Option<StructDeclaratorList>),
    Assert(StaticAssertDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SpecifierQualifierList {
    TypeSpec(TypeSpecifier, Option<Box<Self>>),
    TypeQualif(TypeQualifier, Option<Box<Self>>),
    AlignSpec(AlignmentSpecifier, Option<Box<Self>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclaratorList(pub Vec<StructDeclarator>);

#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarator {
    Declare(Declarator),
    Bitfield(Option<Declarator>, Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumeratorList(pub Vec<Enumerator>);

#[derive(Clone, Debug, PartialEq)]
pub struct Enumerator(pub Identifier, pub Option<Expression>);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncSpecifier {
    Inline,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AlignmentSpecifier {
    AlignAsType(Box<TypeName>),
    AlignAsExpr(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarator(pub Option<Pointer>, pub DirectDeclarator);

#[derive(Clone, Debug, PartialEq)]
pub enum DirectDeclarator {
    Ident(Identifier),
    Declarator(Box<Declarator>),
    Array(Box<Self>, Vec<TypeQualifier>, Option<Expression>),
    ArrayStatic(Box<Self>, Vec<TypeQualifier>, Expression),
    // ArrayStatic2
    FuncPointer(Box<Self>, ParameterList),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pointer(pub Vec<TypeQualifier>, pub Option<Box<Self>>);

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterList {
    pub params: Vec<ParameterDeclaration>,
    pub variadic: bool, // true if parameter list ends with "..."
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParameterDeclaration {
    AbstractDecl(DeclarationSpecifier, Option<AbstractDeclarator>),
    Decl(DeclarationSpecifier, Declarator),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName(pub SpecifierQualifierList, pub Option<AbstractDeclarator>);

#[derive(Clone, Debug, PartialEq)]
pub enum AbstractDeclarator {
    PointerOnly(Pointer),
    Declare(Option<Pointer>, DirectAbstractDeclarator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectAbstractDeclarator {
    Nothing(Box<AbstractDeclarator>),
    Array(Box<Self>, Vec<TypeQualifier>, Option<Expression>),
    ArrayStatic(Box<Self>, Vec<TypeQualifier>, Expression),
    // ArrayStatic2
    FuncPointer(Option<Box<Self>>, ParameterList),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    Assignment(Expression),
    List(InitializerList),
}

#[derive(Clone, Debug, PartialEq)]
pub struct InitializerList(pub Vec<(Option<Designation>, Initializer)>);

#[derive(Clone, Debug, PartialEq)]
pub struct Designation(pub Vec<Designator>);

#[derive(Clone, Debug, PartialEq)]
pub enum Designator {
    Index(Expression),
    Access(Identifier)
}

#[derive(Clone, Debug, PartialEq)]
pub struct StaticAssertDeclaration(pub Expression, pub Literal);

impl<T: TokenStreamer> Parser<T> {
    pub fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        let decl_spec = try_parse_opt!(self.parse_declaration_specifier());

        if let Some((span, decl_spec)) = decl_spec {
            let decl_list = try_parse!(
                self.parse_init_declarator_list(),
                Parsed(_s, p) => p,
                Unhandled => InitDeclaratorList(Vec::new())
            );

            // if we're at the file scope, declaration may end with `{` instead, as is
            // the case with a definition of function
            if matches!(self.current_scope, Scope::File { .. }) {
                if !expect_token_opt!(self, Token::Semicolon).is_obtained() {
                    if !matches!(self.peek(), Some((_, Token::BraceOpen))) {
                        match self.eat() {
                            Obtained(span, tok) =>
                                return ParseResult::Error(span, ParseErr::UnexpectedToken(tok)),
                            EndOfStream(span) =>
                                return ParseResult::Error(span, ParseErr::UnexpectedEnd),
                            _ => unreachable!()
                        };
                    }
                }
            } else {
                expect_token!(self, Token::Semicolon);
            }

            if matches!(decl_spec.0.first().unwrap(), DeclarationSpecifierNode::Storage(StorageClass::Typedef)) {
                let typedef = if let Some(DirectDeclarator::Ident(id)) = &decl_list.0.first().map(|x| &x.0.1) {
                    id.clone()
                } else {
                    return ParseResult::Error(span, ParseErr::InvalidTypedef)
                };
                self.symbol_table.add_symbol(typedef, self.current_scope.clone());
            }

            return ParseResult::Parsed(
                span,
                Declaration::Declare(decl_spec, decl_list)
            )
        }

        let (span, assert) = try_parse!(self.parse_static_assert_declaration());
        ParseResult::Parsed(span, Declaration::StaticAssert(assert))
    }

    pub(super) fn parse_declaration_specifier(&mut self) -> ParseResult<DeclarationSpecifier> {
        let mut span = None;
        let mut list = Vec::new();
        macro_rules! decl_spec_helper {
            ($spec:ident, $spec_func:ident) => {
                let parsed = try_parse_opt!(self.$spec_func());
        
                if parsed.is_some() {
                    let (sp, parsed) = parsed.unwrap();
                    if span == None {
                        span = Some(sp);
                    }
                    list.push(DeclarationSpecifierNode::$spec(parsed));
                    continue
                }
            };
        }

        loop {
            decl_spec_helper!(Storage, parse_storage_class);
            decl_spec_helper!(TypeSpec, parse_type_specifier);
            decl_spec_helper!(TypeQualif, parse_type_qualifier);
            decl_spec_helper!(FuncSpec, parse_function_specifier);
            break;
        }

        if span.is_none() {
            ParseResult::Unhandled
        } else {
            ParseResult::Parsed(span.unwrap(), DeclarationSpecifier(list))
        }
    }

    pub(super) fn parse_init_declarator_list(&mut self) -> ParseResult<InitDeclaratorList> {
        let (span, init) = try_parse!(self.parse_init_declarator());
        let mut list = vec![init];

        while expect_token_opt!(self, Token::Comma).is_obtained() {
            try_parse!(
                self.parse_init_declarator(),
                Parsed(_s, p) => list.push(p),
                Unhandled => return ParseResult::Error(span, ParseErr::UnexpectedToken(Token::Comma))
            );
        }

        ParseResult::Parsed(span, InitDeclaratorList(list))
    }

    pub(super) fn parse_init_declarator(&mut self) -> ParseResult<InitDeclarator> {
        let (span, decl) = try_parse!(self.parse_declarator());

        let init = if expect_token_opt!(self, Token::Equal).is_obtained() {
            let (_span, init) = try_parse!(self.parse_initializer());
            Some(init)
        } else {
            None
        };

        ParseResult::Parsed(span, InitDeclarator(decl, init))
    }

    pub(super) fn parse_storage_class(&mut self) -> ParseResult<StorageClass> {
        let peeked = self.peek();

        let class = match peeked {
            Some((_, Token::Keyword(key))) => match key {
                Keyword::Auto => StorageClass::Auto,
                Keyword::Extern => StorageClass::Extern,
                Keyword::Register => StorageClass::Register,
                Keyword::Static => StorageClass::Static,
                Keyword::Typedef => StorageClass::Typedef,
                _ => return ParseResult::Unhandled
            },

            _ => return ParseResult::Unhandled,
        };

        let span = self.eat().unwrap().0;
        ParseResult::Parsed(span, class)
    }

    pub(super) fn parse_type_specifier(&mut self) -> ParseResult<TypeSpecifier> {
        #[derive(Debug)]
        enum TokenType {
            Simple,
            Struct,
            Union,
            Enum,
            Typedef,
            TypeName,
            Unknown,
        }

        let tok_type = match self.peek() {
            Some((_, Token::Keyword(key))) => match key {
                Keyword::Enum =>  TokenType::Enum,
                Keyword::Struct => TokenType::Struct,
                Keyword::Union => TokenType::Union,
                Keyword::Typedef => TokenType::Typedef,
                Keyword::Bool
                | Keyword::Char
                | Keyword::Double
                | Keyword::Float
                | Keyword::Int
                | Keyword::Long
                | Keyword::Short
                | Keyword::Signed
                | Keyword::Unsigned
                | Keyword::Void
                => TokenType::Simple,
                _ => TokenType::Unknown,
            },

            Some((_, Token::Ident(_))) => TokenType::TypeName,

            _ => TokenType::Unknown,
        };

        let (span, type_spec) = match tok_type {
            TokenType::Simple => {
                let (span, tok) = self.eat().unwrap();
                let type_spec = match tok {
                    Token::Keyword(Keyword::Bool) => TypeSpecifier::Bool,
                    Token::Keyword(Keyword::Char) => TypeSpecifier::Char,
                    Token::Keyword(Keyword::Double) => TypeSpecifier::Double,
                    Token::Keyword(Keyword::Float) => TypeSpecifier::Float,
                    Token::Keyword(Keyword::Int) => TypeSpecifier::Int,
                    Token::Keyword(Keyword::Long) => TypeSpecifier::Long,
                    Token::Keyword(Keyword::Short) => TypeSpecifier::Short,
                    Token::Keyword(Keyword::Signed) => TypeSpecifier::Signed,
                    Token::Keyword(Keyword::Unsigned) => TypeSpecifier::Unsigned,
                    Token::Keyword(Keyword::Void) => TypeSpecifier::Void,
                    _ => unreachable!()
                };
                (span, type_spec)
            },

            TokenType::Enum => {
                let span = self.eat().unwrap().0;
                let ident;
                
                if let Obtained(_, Token::Ident(id)) = expect_token_opt!(self, Token::Ident(_)) {
                    ident = Some(Identifier(id))
                } else {
                    ident = None
                };

                let list;
                if expect_token_opt!(self, Token::BraceOpen).is_obtained() {
                    let parsed_list = try_parse_opt!(self.parse_enumerator_list());

                    list = match parsed_list {
                        Some((_s, p)) => p,
                        None => EnumeratorList(Vec::new()) ,
                    };

                    expect_token_opt!(self, Token::Comma);
                    expect_token!(self, Token::BraceClose);
                } else {
                    if ident.is_none() {
                        return ParseResult::Error(
                            span,
                            ParseErr::UnexpectedToken(Token::Keyword(Keyword::Enum))
                        )
                    }
                    list = EnumeratorList(Vec::new());
                };

                (span, TypeSpecifier::Enum(ident, list))
            },

            tok_ty @ (TokenType::Struct | TokenType::Union) => {
                let span = self.eat().unwrap().0;

                let ident;
                if let Obtained(_, Token::Ident(id)) = expect_token_opt!(self, Token::Ident(_)) {
                    ident = Some(Identifier(id))
                } else {
                    ident = None
                };

                let list;
                if expect_token_opt!(self, Token::BraceOpen).is_obtained() {
                    let parsed_list = try_parse_opt!(self.parse_struct_declaration_list());

                    list = match parsed_list {
                        Some((_s, p)) => p,
                        None => StructDeclarationList(Vec::new()) ,
                    };

                    expect_token!(self, Token::BraceClose);
                } else {
                    let keyword = match tok_ty {
                        TokenType::Struct => Keyword::Struct,
                        TokenType::Union => Keyword::Union,
                        _ => unreachable!()
                    };

                    if ident.is_none() {
                        return ParseResult::Error(
                            span,
                            ParseErr::UnexpectedToken(Token::Keyword(keyword))
                        )
                    }

                    list = StructDeclarationList(Vec::new());
                };

                match tok_ty {
                    TokenType::Struct => (span, TypeSpecifier::Struct(ident, list)),
                    TokenType::Union => (span, TypeSpecifier::Union(ident, list)),
                    _ => unreachable!()
                }
            },

            TokenType::Typedef => {
                let span = self.eat().unwrap().0;
                let ident;
                if let Obtained(_, Token::Ident(id)) = expect_token_opt!(self, Token::Ident(_)) {
                    ident = Identifier(id)
                } else {
                    return ParseResult::Error(span, ParseErr::UnexpectedToken(Token::Keyword(Keyword::Typedef)))
                };
                (span, TypeSpecifier::Typedef(ident))
            }

            TokenType::TypeName => {
                let ident = force_unwrap!(&self.peek().unwrap().1, Token::Ident(i) => Identifier(i.clone()));
                if self.symbol_table.is_ident_a_type(&ident, &self.current_scope) {
                    let (span, ident) = self.eat().unwrap();
                    let ident = force_unwrap!(ident, Token::Ident(id) => Identifier(id));
                    (span, TypeSpecifier::TypeName(ident))
                } else {
                    return ParseResult::Unhandled
                }
            }

            TokenType::Unknown => {
                return ParseResult::Unhandled
            }
        };

        ParseResult::Parsed(span, type_spec)
    }

    pub(super) fn parse_struct_declaration_list(&mut self) -> ParseResult<StructDeclarationList> {
        let (span, decl) = try_parse!(self.parse_struct_declaration());
        let mut list = Vec::with_capacity(1);

        let mut decl = Some(decl);
        while let Some(d) = decl {
            list.push(d);
            decl = try_parse_opt!(self.parse_struct_declaration()).map(|(_s, p)| p);
        }

        ParseResult::Parsed(span, StructDeclarationList(list))
    }

    pub(super) fn parse_struct_declaration(&mut self) -> ParseResult<StructDeclaration> {
        let qualifs = try_parse_opt!(self.parse_specifier_qualifier_list());

        if let Some((s, p)) = qualifs {
            let decl_list = try_parse_opt!(self.parse_struct_declarator_list()).map(|(_, p)| p);
            expect_token!(self, Token::Semicolon);

            // consume excess semicolons
            while expect_token_opt!(self, Token::Semicolon).is_obtained() {}

            let result = StructDeclaration::Field(p, decl_list);
            return ParseResult::Parsed(s, result)
        };

        let asserted = try_parse_opt!(self.parse_static_assert_declaration());

        if asserted.is_some() {
            let (span, assert) = asserted.unwrap();
            ParseResult::Parsed(span, StructDeclaration::Assert(assert))
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_specifier_qualifier_list(&mut self) -> ParseResult<SpecifierQualifierList> {
        let result = if let Some((s, p)) = try_parse_opt!(self.parse_type_specifier()) {
            (s, SpecifierQualifierList::TypeSpec(p, None))
        } else if let Some((s, p)) = try_parse_opt!(self.parse_type_qualifier()) {
            (s, SpecifierQualifierList::TypeQualif(p, None))
        } else if let Some((s, p)) = try_parse_opt!(self.parse_alignment_specifier()) {
            (s, SpecifierQualifierList::AlignSpec(p, None))
        } else {
            return ParseResult::Unhandled
        };

        let qualif_list = try_parse_opt!(self.parse_specifier_qualifier_list())
            .map(|(_s, p)| Box::new(p));

        match result {
            (s, SpecifierQualifierList::TypeSpec(p, _)) =>
                ParseResult::Parsed(s, SpecifierQualifierList::TypeSpec(p, qualif_list)),
            (s, SpecifierQualifierList::TypeQualif(p, _)) =>
                ParseResult::Parsed(s, SpecifierQualifierList::TypeQualif(p, qualif_list)),
            (s, SpecifierQualifierList::AlignSpec(p, _)) =>
                ParseResult::Parsed(s, SpecifierQualifierList::AlignSpec(p, qualif_list)),
        }
    }

    pub(super) fn parse_struct_declarator_list(&mut self) -> ParseResult<StructDeclaratorList> {
        let (span, decl) = try_parse!(self.parse_struct_declarator());
        let mut list = vec![decl];

        while let Obtained(span, _) = expect_token_opt!(self, Token::Comma) {
            try_parse!(
                self.parse_struct_declarator(),
                Parsed(_s, p) => list.push(p),
                Unhandled => return ParseResult::Error(span, ParseErr::UnexpectedToken(Token::Comma))
            );
        }

        ParseResult::Parsed(span, StructDeclaratorList(list))
    }

    pub(super) fn parse_struct_declarator(&mut self) -> ParseResult<StructDeclarator> {
        let decl = try_parse_opt!(self.parse_declarator());

        if expect_token_opt!(self, Token::Colon).is_obtained() {
            let (span, expr) = try_parse!(self.parse_conditional_expression());
            let expr = Expression::from_comma_expression(wrap!(conditional expr));

            let result = StructDeclarator::Bitfield(decl.map(|(_s, p)| p), expr);
            ParseResult::Parsed(span, result)
        } else {
            if decl.is_some() {
                let (span, decl) = decl.unwrap();
                let result = StructDeclarator::Declare(decl);
                ParseResult::Parsed(span, result)
            } else {
                ParseResult::Unhandled
            }
        }
    }

    pub(super) fn parse_enumerator_list(&mut self) -> ParseResult<EnumeratorList> {
        let (span, enumerator) = try_parse!(self.parse_enumerator());
        let mut list = vec![enumerator];

        while let Obtained(span, _) = expect_token_opt!(self, Token::Comma) {
            try_parse!(
                self.parse_enumerator(),
                Parsed(_s, p) => list.push(p),
                Unhandled => {
                    self.backlog.push_front((span, Token::Comma));
                    break
                }
            );
        }

        ParseResult::Parsed(span, EnumeratorList(list))
    }

    pub(super) fn parse_enumerator(&mut self) -> ParseResult<Enumerator> {
        if let Obtained(span, Token::Ident(id)) = expect_token_opt!(self, Token::Ident(_)) {
            expect_token!(self, Token::Equal);
            let cond = try_parse_opt!(self.parse_conditional_expression())
                .map(|(_s, p)| p)
                .map(|expr| Expression::from_comma_expression(wrap!(conditional expr))
            );
            ParseResult::Parsed(span, Enumerator(Identifier(id), cond))
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_type_qualifier(&mut self) -> ParseResult<TypeQualifier> {
        if let Obtained(span, _) = expect_token_opt!(self, Token::Keyword(Keyword::Const)) {
            ParseResult::Parsed(span, TypeQualifier::Const)
        } else if let Obtained(span, _) = expect_token_opt!(self, Token::Keyword(Keyword::Restrict)) {
            ParseResult::Parsed(span, TypeQualifier::Restrict)
        } else if let Obtained(span, _) = expect_token_opt!(self, Token::Keyword(Keyword::Volatile)) {
            ParseResult::Parsed(span, TypeQualifier::Volatile)
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_function_specifier(&mut self) -> ParseResult<FuncSpecifier> {
        if let Obtained(span, _) = expect_token_opt!(self, Token::Ident(id) if id == "inline") {
            ParseResult::Parsed(span, FuncSpecifier::Inline)
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_alignment_specifier(&mut self) -> ParseResult<AlignmentSpecifier> {
        if expect_token_opt!(self, Token::Ident(id) if id == "_Alignas").is_obtained() {
            expect_token!(self, Token::ParenOpen);

            let type_name = try_parse_opt!(self.parse_type_name());

            let (span, result) = if type_name.is_none() {
                let (span, expr) = try_parse!(
                    self.parse_conditional_expression(),
                    Parsed(s, p) => (s, wrap!(conditional p)),
                    Unhandled => {
                        let (span, tok) = self.eat().unwrap();
                        return ParseResult::Error(span, ParseErr::UnexpectedToken(tok))
                    }
                );
                let expr = Expression::from_comma_expression(expr);
                (span, AlignmentSpecifier::AlignAsExpr(expr))
            } else {
                let (span, type_name) = type_name.unwrap();
                (span, AlignmentSpecifier::AlignAsType(Box::new(type_name)))
            };

            expect_token!(self, Token::ParenClose);

            ParseResult::Parsed(span, result)
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_declarator(&mut self) -> ParseResult<Declarator> {
        let ptr = try_parse_opt!(self.parse_pointer()).map(|(_s, p)| p);

        let (span, decl) = try_parse!(self.parse_direct_declarator());
        ParseResult::Parsed(span, Declarator(ptr, decl))
    }

    pub(super) fn parse_direct_declarator(&mut self) -> ParseResult<DirectDeclarator> {
        let (span, left) = if expect_token_opt!(self, Token::ParenOpen).is_obtained() {
            let (span, decl) = try_parse!(self.parse_declarator());
            expect_token!(self, Token::ParenClose);
            (span, DirectDeclarator::Declarator(Box::new(decl)))
        } else if let Obtained(span, Token::Ident(id)) = expect_token_opt!(self, Token::Ident(_)) {
            (span, DirectDeclarator::Ident(Identifier(id)))
        } else {
            return ParseResult::Unhandled
        };

        self.parse_direct_declarator_right((span, left))
    }

    pub fn parse_direct_declarator_right(&mut self, (span, left): (Span, DirectDeclarator)) 
        -> ParseResult<DirectDeclarator> 
    {
        if let Obtained(span, _) = expect_token_opt!(self, Token::BracketOpen) {
            let front_static = expect_token_opt!(self, Token::Keyword(Keyword::Static));
            
            if let EndOfStream(span) = front_static {
                return ParseResult::Error(span, ParseErr::UnexpectedEnd)
            };

            let mut type_qualifs = Vec::new();
            loop {
                try_parse!(
                    self.parse_type_qualifier(),
                    Parsed(_s, p) => type_qualifs.push(p),
                    Unhandled => break
                );
            }

            let mid_static = expect_token_opt!(self, Token::Keyword(Keyword::Static));

            if let Obtained(ref span, ref tok) = mid_static {
                if front_static.is_obtained() {
                    return ParseResult::Error(
                        span.clone(),
                        ParseErr::UnexpectedToken(tok.clone())
                    )
                }
            };

            let assign = try_parse!(
                self.parse_assignment_expression(),
                Parsed(_s, p) => Some(wrap!(assignment p)),
                Unhandled => None
            );

            let assign = assign.map(|x| Expression::from_comma_expression(x));

            expect_token!(self, Token::BracketClose);

            let left = if !front_static.is_obtained() && !mid_static.is_obtained() {
                DirectDeclarator::Array(Box::new(left), type_qualifs, assign)
            } else if front_static.is_obtained() && assign.is_some() {
                DirectDeclarator::ArrayStatic(Box::new(left), type_qualifs, assign.unwrap())
            } else if mid_static.is_obtained() && !type_qualifs.is_empty() && assign.is_some() {
                DirectDeclarator::ArrayStatic(Box::new(left), type_qualifs, assign.unwrap())
            } else {
                return ParseResult::Error(span, ParseErr::InvalidDirectAbstractDeclarator)
            };

            self.parse_direct_declarator_right((span, left))
        } else if expect_token_opt!(self, Token::ParenOpen).is_obtained() {
            let params = try_parse_opt!(self.parse_parameter_list());
            expect_token!(self, Token::ParenClose);

            if let Some((span, params)) = params {
                let left = DirectDeclarator::FuncPointer(Box::new(left), params);
                self.parse_direct_declarator_right((span, left))
            } else {
                let left = DirectDeclarator::FuncPointer(Box::new(left), ParameterList {
                    params: Vec::new(),
                    variadic: false,
                });
                self.parse_direct_declarator_right((span, left))
            }
        } else {
            ParseResult::Parsed(span, left)
        }
    }

    pub(super) fn parse_pointer(&mut self) -> ParseResult<Pointer> {
        if let Obtained(span, _) = expect_token_opt!(self, Token::Multiply) {
            let mut type_qualifs = Vec::new();
            loop {
                try_parse!(
                    self.parse_type_qualifier(),
                    Parsed(_s, p) => type_qualifs.push(p),
                    Unhandled => break
                );
            }

            let ptr = match self.parse_pointer() {
                ParseResult::Parsed(_s, p) => Pointer(type_qualifs, Some(Box::new(p))),
                e @ ParseResult::Error(_, _) => return e,
                ParseResult::Unhandled => Pointer(type_qualifs, None),
            };

            ParseResult::Parsed(span, ptr)
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_parameter_list(&mut self) -> ParseResult<ParameterList> {
        let (span, param) = try_parse!(self.parse_parameter_declaration());
        let mut params = vec![param];

        let mut possible_variadic_span = None;

        while self.eat_if(|(_,tok)| matches!(tok, Token::Comma)).is_obtained() {
            try_parse!(
                self.parse_parameter_declaration(),
                Parsed(_s, p) => params.push(p),
                Unhandled => match self.eat() {
                    Obtained(s, Token::Dot) => possible_variadic_span = Some(s),
                    Obtained(s, tok) => return ParseResult::Error(s, ParseErr::UnexpectedToken(tok)),
                    EndOfStream(span) => return ParseResult::Error(span, ParseErr::UnexpectedEnd),
                    _ => unreachable!()
                }
            );
        }

        let mut variadic = false;
        if possible_variadic_span.is_some() {
            if let Some((span1, Token::Dot)) = self.peek_nth(0).cloned() {
                if let Some((span2, Token::Dot)) = self.peek_nth(1).cloned() {
                    let span0 = possible_variadic_span.unwrap();

                    let same_line = span0.line == span1.line
                        && span1.line == span2.line;
                    let consecutive = span0.column + 1 == span1.column
                        && span1.column + 1 == span2.column;

                    variadic = same_line && consecutive;

                    self.eat();
                    self.eat();
                }
            }
        }

        ParseResult::Parsed(span, ParameterList { params, variadic })
    }

    pub(super) fn parse_parameter_declaration(&mut self) -> ParseResult<ParameterDeclaration> {
        let (span, decl_spec) = try_parse!(self.parse_declaration_specifier());
        
        let decl = try_parse_opt!(self.parse_declarator());

        if decl.is_some() {
            let (span, decl) = decl.unwrap();
            let result = ParameterDeclaration::Decl(decl_spec, decl);
            return ParseResult::Parsed(span, result)
        };

        let abst = try_parse_opt!(self.parse_abstract_declarator()).map(|(_s, p)| p);

        let result = ParameterDeclaration::AbstractDecl(decl_spec, abst);
        ParseResult::Parsed(span, result)
    }

    pub(super) fn parse_type_name(&mut self) -> ParseResult<TypeName> {
        let (span, spec) = try_parse!(self.parse_specifier_qualifier_list());
        let abst = try_parse_opt!(self.parse_abstract_declarator()).map(|(_s, p)| p);
        ParseResult::Parsed(span, TypeName(spec, abst))
    }

    pub(super) fn parse_abstract_declarator(&mut self) -> ParseResult<AbstractDeclarator> {
        let pointer = try_parse_opt!(self.parse_pointer());

        let decl = try_parse_opt!(self.parse_direct_abstract_declarator());

        if decl.is_some() {
            let (span, decl) = decl.unwrap();
            let decl = AbstractDeclarator::Declare(pointer.map(|(_, p)| p), decl);
            ParseResult::Parsed(span, decl)
        } else if pointer.is_some() {
            let (span, pointer) = pointer.unwrap();
            ParseResult::Parsed(span, AbstractDeclarator::PointerOnly(pointer))
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_direct_abstract_declarator(&mut self) -> ParseResult<DirectAbstractDeclarator> {
        let (span, left) = if expect_token_opt!(self, Token::ParenOpen).is_obtained() {
            let abst = try_parse_opt!(self.parse_abstract_declarator());

            if abst.is_none() {
                let (span, params) = try_parse!(self.parse_parameter_list());
                expect_token!(self, Token::ParenClose);
                (span, DirectAbstractDeclarator::FuncPointer(None, params))
            } else {
                let (span, abst) = abst.unwrap();
                expect_token!(self, Token::ParenClose);
                (span, DirectAbstractDeclarator::Nothing(Box::new(abst)))
            }
        } else {
            return ParseResult::Unhandled
        };

        self.parse_direct_abstract_declarator_right((span, left))
    }

    pub fn parse_direct_abstract_declarator_right(&mut self, (span, left): (Span, DirectAbstractDeclarator)) 
        -> ParseResult<DirectAbstractDeclarator> 
    {
        if let Obtained(span, _) = expect_token_opt!(self, Token::BracketOpen) {
            let front_static = expect_token_opt!(self, Token::Keyword(Keyword::Static));
            
            if let EndOfStream(span) = front_static {
                return ParseResult::Error(span, ParseErr::UnexpectedEnd)
            };

            let mut type_qualifs = Vec::new();
            loop {
                try_parse!(
                    self.parse_type_qualifier(),
                    Parsed(_s, p) => type_qualifs.push(p),
                    Unhandled => break
                );
            }

            let mid_static = expect_token_opt!(self, Token::Keyword(Keyword::Static));

            if let Obtained(ref span, ref tok) = mid_static {
                if front_static.is_obtained() {
                    return ParseResult::Error(
                        span.clone(),
                        ParseErr::UnexpectedToken(tok.clone())
                    )
                }
            };

            let assign = try_parse!(
                self.parse_assignment_expression(),
                Parsed(_s, p) => Some(wrap!(assignment p)),
                Unhandled => None
            );

            let assign = assign.map(|x| Expression::from_comma_expression(x));

            expect_token!(self, Token::BracketClose);

            let left = if !front_static.is_obtained() && !mid_static.is_obtained() {
                DirectAbstractDeclarator::Array(Box::new(left), type_qualifs, assign)
            } else if front_static.is_obtained() && assign.is_some() {
                DirectAbstractDeclarator::ArrayStatic(Box::new(left), type_qualifs, assign.unwrap())
            } else if mid_static.is_obtained() && !type_qualifs.is_empty() && assign.is_some() {
                DirectAbstractDeclarator::ArrayStatic(Box::new(left), type_qualifs, assign.unwrap())
            } else {
                return ParseResult::Error(span, ParseErr::InvalidDirectAbstractDeclarator)
            };

            self.parse_direct_abstract_declarator_right((span, left))
        } else if expect_token_opt!(self, Token::ParenOpen).is_obtained() {
            let (span, params) = try_parse!(
                self.parse_parameter_list(),
                Parsed(s, p) => (s, p),
                Unhandled => return ParseResult::Parsed(span, left)
            );
            expect_token!(self, Token::ParenClose);
            let left = DirectAbstractDeclarator::FuncPointer(Some(Box::new(left)), params);
            self.parse_direct_abstract_declarator_right((span, left))
        } else {
            ParseResult::Parsed(span, left)
        }
    }

    pub fn parse_initializer(&mut self) -> ParseResult<Initializer> {
        if expect_token_opt!(self, Token::BraceOpen).is_obtained() {
            let (span, list) = try_parse!(self.parse_initializer_list());
            expect_token_opt!(self, Token::Comma);
            expect_token!(self, Token::BraceClose);
            ParseResult::Parsed(span, Initializer::List(list))
        } else {
            let (span, assign) = try_parse!(self.parse_assignment_expression());
            let assign = Expression::from_comma_expression(wrap!(assignment assign));
            ParseResult::Parsed(span, Initializer::Assignment(assign))
        }
    }

    pub(super) fn parse_initializer_list(&mut self) -> ParseResult<InitializerList> {
        let desg = try_parse_opt!(self.parse_designation()).map(|(_s, p)| p);
        let (span, init) = try_parse!(self.parse_initializer());
        let list = InitializerList(vec![(desg, init)]);
        self.parse_initializer_list_right((span, list))
    }

    pub fn parse_initializer_list_right(&mut self, (span, list): (Span, InitializerList))
        -> ParseResult<InitializerList> 
    {
        let mut span = span;
        let InitializerList(mut list) = list;

        if let Obtained(comma_span, _) = expect_token_opt!(self, Token::Comma) {
            let desg = try_parse_opt!(self.parse_designation()).map(|(_s, p)| p);

            let (s, init) = if desg.is_some() {
                try_parse!(self.parse_initializer())
            } else {
                try_parse!(
                    self.parse_initializer(),
                    Parsed(s, p) => (s, p),
                    Unhandled => {
                        self.backlog.push_front((comma_span, Token::Comma));
                        return ParseResult::Parsed(span, InitializerList(list))
                    }
                )
            };

            span = s;
            list.push((desg, init));

            self.parse_initializer_list_right((span, InitializerList(list)))
        } else {
            ParseResult::Parsed(span, InitializerList(list))
        }
    }

    pub(super) fn parse_designation(&mut self) -> ParseResult<Designation> {
        let mut designators = Vec::new();
        let mut span = Span::default();

        while let ParseResult::Parsed(s, p) = self.parse_designator() {
            designators.push(p);
            span = s;
        }

        if !designators.is_empty() {
            expect_token!(self, Token::Equal);
            ParseResult::Parsed(span, Designation(designators))
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_designator(&mut self) -> ParseResult<Designator> {
        if expect_token_opt!(self, Token::BracketOpen).is_obtained() {
            let (span, cond) = try_parse!(self.parse_conditional_expression());
            let cond = Expression::from_comma_expression(wrap!(conditional cond));

            expect_token!(self, Token::BracketClose);
            ParseResult::Parsed(span, Designator::Index(cond))
        } else if expect_token_opt!(self, Token::Dot).is_obtained() {
            let (span, ident) = match expect_token_opt!(self, Token::Ident(_)) {
                Obtained(s, Token::Ident(ident)) => (s, Identifier(ident)),
                EndOfStream(s) => return ParseResult::Error(s, ParseErr::UnexpectedEnd),
                _ => return ParseResult::Unhandled,
            };
            ParseResult::Parsed(span, Designator::Access(ident))
        } else {
            ParseResult::Unhandled
        }
    }

    pub(super) fn parse_static_assert_declaration(&mut self) -> ParseResult<StaticAssertDeclaration> {
        if expect_token_opt!(self, Token::Ident(id) if id == "_Static_assert").is_obtained() {
            expect_token!(self, Token::ParenOpen);
            let (cond_span, cond_expr) = try_parse!(self.parse_conditional_expression());
            let cond_expr = Expression::from_comma_expression(wrap!(conditional cond_expr));
            expect_token!(self, Token::Comma);
            let (_str_span, str) = try_parse!(self.parse_string());
            expect_token!(self, Token::ParenClose);
            expect_token!(self, Token::Semicolon);
            ParseResult::Parsed(cond_span, StaticAssertDeclaration(cond_expr, str))
        } else {
            ParseResult::Unhandled
        }
    }
}
