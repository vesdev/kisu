use crate::{lexer::TokenKind, types::Type};
use bumpalo::collections::Vec;
use logos::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeIdent {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding<'ast> {
    pub kind: BindingKind,
    pub ident: Ident,
    pub constraint: Option<Type>,
    pub expr: &'ast Expr<'ast>,
    pub span: Span,
}

impl<'ast> Binding<'ast> {
    pub fn new(ident: Ident, constraint: Option<Type>, expr: &'ast Expr<'ast>, span: Span) -> Self {
        Self {
            kind: BindingKind::Normal,
            ident,
            constraint,
            expr,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BindingKind {
    Normal,
    Rec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'ast> {
    pub ident: Ident,
    pub constraint: Option<Type>,
    pub expr: Option<&'ast Expr<'ast>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub ident: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef<'ast> {
    pub name: TypeIdent,
    pub fields: Vec<'ast, StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'ast> {
    pub structs: Vec<'ast, StructDef<'ast>>,
    pub expr: Expr<'ast>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Str(pub String, pub Span);

#[derive(Debug, Clone, PartialEq)]
pub struct Num(pub f64, pub Span);

#[derive(Debug, Clone, PartialEq)]
pub struct List<'ast> {
    pub exprs: Vec<'ast, Expr<'ast>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'ast> {
    Number(Num),
    String(Str),
    Bool(bool),
    List(List<'ast>),
    Ident(Ident),
    TypeIdent(TypeIdent),
    Unary {
        op: UnaryOp,
        expr: &'ast Expr<'ast>,
        span: Span,
    },
    Binary {
        op: BinaryOp,
        lhs: &'ast Expr<'ast>,
        rhs: &'ast Expr<'ast>,
        span: Span,
    },
    StructAccess {
        expr: &'ast Expr<'ast>,
        ident: Ident,
        span: Span,
    },
    Lambda {
        params: Vec<'ast, Param<'ast>>,
        body: &'ast Expr<'ast>,
        span: Span,
    },
    Block {
        bindings: Vec<'ast, Binding<'ast>>,
        expr: &'ast Expr<'ast>,
        span: Span,
    },
    Struct {
        type_name: TypeIdent,
        fields: Vec<'ast, Binding<'ast>>,
        span: Span,
    },
    App {
        lhs: &'ast Expr<'ast>,
        rhs: &'ast Expr<'ast>,
        span: Span,
    },
    IfExpr {
        cond: &'ast Expr<'ast>,
        then_expr: &'ast Expr<'ast>,
        else_expr: &'ast Expr<'ast>,
        span: Span,
    },
}

impl<'ast> Expr<'ast> {
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(n) => n.1.clone(),
            Expr::String(s) => s.1.clone(),
            Expr::Bool(_) => 0..0,
            Expr::List(list) => list.span.clone(),
            Expr::Ident(ident) => ident.span.clone(),
            Expr::Unary { span, .. } => span.clone(),
            Expr::Binary { span, .. } => span.clone(),
            Expr::StructAccess { span, .. } => span.clone(),
            Expr::Lambda { span, .. } => span.clone(),
            Expr::Block { span, .. } => span.clone(),
            Expr::Struct { span, .. } => span.clone(),
            Expr::App { span, .. } => span.clone(),
            Expr::IfExpr { span, .. } => span.clone(),
            Expr::TypeIdent(TypeIdent { span, .. }) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Dot,
}

impl BinaryOp {
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Dot => 5,
            BinaryOp::Mul | BinaryOp::Div => 4,
            BinaryOp::Add | BinaryOp::Sub => 3,
            BinaryOp::Eq
            | BinaryOp::NotEq
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => 2,
        }
    }

    pub fn token_kind(&self) -> TokenKind {
        match self {
            BinaryOp::Add => TokenKind::Add,
            BinaryOp::Sub => TokenKind::Sub,
            BinaryOp::Mul => TokenKind::Mul,
            BinaryOp::Div => TokenKind::Div,
            BinaryOp::Eq => TokenKind::Eq,
            BinaryOp::NotEq => TokenKind::NotEq,
            BinaryOp::Lt => TokenKind::Lt,
            BinaryOp::Gt => TokenKind::Gt,
            BinaryOp::LtEq => TokenKind::LtEq,
            BinaryOp::GtEq => TokenKind::GtEq,
            BinaryOp::Dot => TokenKind::Dot,
        }
    }
}

pub trait Visitor<'ast>: Sized {
    type Err;

    fn visit_program(&mut self, program: &'ast Program<'ast>) -> Result<(), Self::Err> {
        for s in &program.structs {
            self.visit_struct_def(s)?;
        }
        self.visit_expr(&program.expr)
    }

    fn visit_expr(&mut self, expr: &'ast Expr<'ast>) -> Result<(), Self::Err> {
        match expr {
            Expr::Number(n) => self.visit_num(n),
            Expr::String(s) => self.visit_str(s),
            Expr::Bool(b) => self.visit_bool(*b),
            Expr::Unary { op, expr, span: _ } => self.visit_unary_op(op, expr),
            Expr::Binary {
                op,
                lhs,
                rhs,
                span: _,
            } => self.visit_binary_op(op, lhs, rhs),
            Expr::Lambda {
                params,
                body,
                span: _,
            } => self.visit_lambda(params, body),
            Expr::Block {
                bindings,
                expr,
                span: _,
            } => self.visit_block_expr(bindings, expr),
            Expr::Struct {
                type_name,
                fields,
                span: _,
            } => self.visit_struct_expr(type_name, fields),
            Expr::App { lhs, rhs, span: _ } => self.visit_app(lhs, rhs),
            Expr::Ident(ident) => self.visit_ident(ident),
            Expr::List(list) => self.visit_list(list),
            Expr::StructAccess {
                expr,
                ident,
                span: _,
            } => self.visit_struct_access(expr, ident),
            Expr::IfExpr {
                cond,
                then_expr,
                else_expr,
                span: _,
            } => self.visit_if_expr(cond, then_expr, else_expr),
            Expr::TypeIdent(_) => todo!(),
        }
    }

    fn visit_ident(&mut self, ident: &'ast Ident) -> Result<(), Self::Err>;
    fn visit_bind(&mut self, bind: &'ast Binding<'ast>) -> Result<(), Self::Err>;
    fn visit_num(&mut self, num: &'ast Num) -> Result<(), Self::Err>;
    fn visit_str(&mut self, str: &'ast Str) -> Result<(), Self::Err>;
    fn visit_bool(&mut self, b: bool) -> Result<(), Self::Err>;
    fn visit_unary_op(
        &mut self,
        op: &'ast UnaryOp,
        expr: &'ast Expr<'ast>,
    ) -> Result<(), Self::Err>;
    fn visit_binary_op(
        &mut self,
        op: &'ast BinaryOp,
        lhs: &'ast Expr<'ast>,
        rhs: &'ast Expr<'ast>,
    ) -> Result<(), Self::Err>;
    fn visit_struct_expr(
        &mut self,
        type_name: &'ast TypeIdent,
        fields: &'ast [Binding<'ast>],
    ) -> Result<(), Self::Err>;
    fn visit_struct_def(&mut self, struct_def: &'ast StructDef<'ast>) -> Result<(), Self::Err>;
    fn visit_block_expr(
        &mut self,
        bindings: &'ast [Binding<'ast>],
        expr: &'ast Expr<'ast>,
    ) -> Result<(), Self::Err>;
    fn visit_app(&mut self, lhs: &'ast Expr<'ast>, rhs: &'ast Expr<'ast>) -> Result<(), Self::Err>;
    fn visit_lambda(
        &mut self,
        params: &'ast [Param<'ast>],
        body: &'ast Expr<'ast>,
    ) -> Result<(), Self::Err>;
    fn visit_list(&mut self, list: &'ast List<'ast>) -> Result<(), Self::Err>;
    fn visit_struct_access(
        &mut self,
        expr: &'ast Expr<'ast>,
        ident: &'ast Ident,
    ) -> Result<(), Self::Err>;
    fn visit_if_expr(
        &mut self,
        cond: &'ast Expr<'ast>,
        then_expr: &'ast Expr<'ast>,
        else_expr: &'ast Expr<'ast>,
    ) -> Result<(), Self::Err>;
    fn visit_type(&mut self, ty: &'ast Type) -> Result<(), Self::Err>;
}
