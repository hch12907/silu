use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Primary(PrimaryExpr),
    PostIncrement(Box<Self>),
    PostDecrement(Box<Self>),
    Access(Box<Self>, Identifier),
    AccessDeref(Box<Self>, Identifier),
    Call(Box<Self>, Vec<Self>),
    Index(Box<Self>, Box<Self>), 
    PreIncrement(Box<Self>),
    PreDecrement(Box<Self>),
    Positive(Box<Self>),
    Negative(Box<Self>),
    LogicNot(Box<Self>),
    BitNot(Box<Self>),
    Cast(Box<TypeName>, Box<Self>),
    Deref(Box<Self>),
    Reference(Box<Self>),
    Sizeof(Box<Self>),
    SizeofType(Box<TypeName>),
    Binary(BinaryOperator, Box<Self>, Box<Self>),
    Cond(Box<Self>, Box<Self>, Box<Self>),
    Assign(Box<Self>, Box<Self>),
    OpAssign(AssignOperator, Box<Self>, Box<Self>),
    Comma(Box<Self>, Box<Self>),
}

impl Expression {
    pub fn from_comma_expression(expr: CommaExpression) -> Expression {
        let make_expr = |expr| Box::new(Self::from_comma_expression(expr));

        let assign = { 
            use crate::parser::CommaExpression::*;
            match expr {
                Comma(expr, assign) => 
                    return Self::Comma(
                        make_expr(*expr),
                        make_expr(wrap!(assignment assign))
                    ),
            
                Nothing(assign) => assign
            }
        };

        let cond = {
            use crate::parser::AssignmentExpr::*;
            match assign {
                Assign(pre, assign) => return Self::Assign(
                    make_expr(wrap!(prefix pre)),
                    make_expr(wrap!(assignment *assign))
                ),

                OpAssign(op, pre, assign) => return Self::OpAssign(
                    op,
                    make_expr(wrap!(prefix pre)),
                    make_expr(wrap!(assignment *assign))
                ),

                Nothing(cond) => cond,
            }
        };

        let binary = {
            use crate::parser::ConditionalExpr::*;
            match cond {
                Cond(bin, expr, cond) => return Self::Cond(
                    make_expr(wrap!(binary bin)),
                    make_expr(wrap!(expression *expr)),
                    make_expr(wrap!(conditional *cond)),
                ),

                Nothing(bin) => bin,
            }
        };

        let prefix = {
            use crate::parser::BinaryExpr::*;
            match binary {
                Binary(op, bin_l, bin_r) => return Self::Binary(
                    op,
                    make_expr(wrap!(binary *bin_l)),
                    make_expr(wrap!(binary *bin_r)),
                ),

                Nothing(pre) => pre,
            }
        };

        let postfix = {
            use crate::parser::PrefixExpr::*;
            match prefix {
                PreIncrement(pre) => return Self::PreIncrement(
                    make_expr(wrap!(prefix *pre))
                ),
                PreDecrement(pre) => return Self::PreDecrement(
                    make_expr(wrap!(prefix *pre))
                ),
                Positive(pre) => return Self::Positive(
                    make_expr(wrap!(prefix *pre))
                ),
                Negative(pre) => return Self::Negative(
                    make_expr(wrap!(prefix *pre))
                ),
                LogicNot(pre) => return Self::LogicNot(
                    make_expr(wrap!(prefix *pre))
                ),
                BitNot(pre) => return Self::BitNot(
                    make_expr(wrap!(prefix *pre))
                ),
                Cast(ty, pre) => return Self::Cast(
                    Box::new(ty),
                    make_expr(wrap!(prefix *pre))
                ),
                Deref(pre) => return Self::Deref(
                    make_expr(wrap!(prefix *pre))
                ),
                Reference(pre) => return Self::Reference(
                    make_expr(wrap!(prefix *pre))
                ),
                Sizeof(pre) => return Self::Sizeof(
                    make_expr(wrap!(prefix *pre))
                ), 
                SizeofType(ty) => return Self::SizeofType(
                    Box::new(ty)
                ),
                Nothing(post) => post,
            }
        };
 
        let primary = {
            use crate::parser::PostfixExpr::*;
            match postfix {        
                PostIncrement(post) => return Self::PostIncrement(
                    make_expr(wrap!(postfix *post))
                ),
                PostDecrement(post) => return Self::PostDecrement(
                    make_expr(wrap!(postfix *post))
                ),
                Access(post, id) => return Self::Access(
                    make_expr(wrap!(postfix *post)), 
                    id
                ),
                AccessDeref(post, id) => return Self::AccessDeref(
                    make_expr(wrap!(postfix *post)),
                    id
                ),
                Call(post, args) => return Self::Call(
                    make_expr(wrap!(postfix *post)),
                    {
                        let ArgumentExpr(args) = args;
                        args.into_iter()
                            .map(|arg| Self::from_comma_expression(wrap!(assignment arg)))
                            .collect::<Vec<_>>()
                    }
                ),
                Index(post, expr) => return Self::Index(
                    make_expr(wrap!(postfix *post)),
                    make_expr(wrap!(expression *expr))
                ),
                Nothing(prim) => prim,
            }
        };

        match primary {
            PrimaryExpr::Grouped(expr) => Self::from_comma_expression(*expr),
            p => Expression::Primary(p),
        }
    }
}
