use crate::types::Type;
use logos::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BindingKind {
    Normal,
    Rec,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Str(pub String, pub Span, pub Type);

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeIdent {
    pub name: String,
    pub span: Span,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub ident: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: TypeIdent,
    pub fields: Vec<StructField>,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub kind: BindingKind,
    pub ident: Ident,
    pub expr: Box<Expr>,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ident: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expr: Expr,
    pub structs: Vec<StructDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Num(pub f64, pub Span, pub Type);

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub exprs: Vec<Expr>,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub span: Span,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Number(Num),
    String(Str),
    Bool(bool),
    List(List),
    Ident(Ident),
    Unary {
        op: UnaryOp,
        expr: Expr,
    },
    Binary {
        op: BinaryOp,
        lhs: Expr,
        rhs: Expr,
        ty: Type,
    },
    StructAccess {
        expr: Expr,
        ident: Ident,
    },
    Lambda {
        params: Vec<Param>,
        body: Expr,
    },
    Block {
        bindings: Vec<Binding>,
        expr: Expr,
    },
    Struct {
        fields: Vec<Binding>,
        ty: Type,
    },
    App {
        lhs: Expr,
        rhs: Expr,
    },
    IfExpr {
        cond: Expr,
        then_expr: Expr,
        else_expr: Expr,
    },
}

#[allow(clippy::missing_safety_doc)]
pub trait Visitor<'ast>: Sized {
    unsafe fn visit_program(&mut self, program: &'ast Program) {
        unsafe {
            for s in &program.structs {
                self.visit_struct_def(s);
            }
            self.visit_expr(&program.expr)
        }
    }

    unsafe fn visit_expr(&mut self, expr: &'ast Expr) {
        unsafe {
            match expr.kind.as_ref() {
                ExprKind::Number(n) => self.visit_num(n),
                ExprKind::String(s) => self.visit_str(s),
                ExprKind::Bool(b) => self.visit_bool(*b),
                ExprKind::List(l) => self.visit_list(l),
                ExprKind::Ident(i) => self.visit_ident(i),
                ExprKind::Unary { op, expr } => self.visit_unary_op(op, expr),
                ExprKind::Binary { op, lhs, rhs, ty } => self.visit_binary_op(op, lhs, rhs, ty),
                ExprKind::StructAccess { expr, ident } => self.visit_struct_access(expr, ident),
                ExprKind::Lambda { params, body } => self.visit_lambda(params, body),
                ExprKind::Block { bindings, expr } => self.visit_block_expr(bindings, expr),
                ExprKind::Struct { fields, ty } => self.visit_struct_expr(fields, ty),
                ExprKind::App { lhs, rhs } => self.visit_app(lhs, rhs),
                ExprKind::IfExpr {
                    cond,
                    then_expr,
                    else_expr,
                } => self.visit_if_expr(cond, then_expr, else_expr),
            }
        }
    }

    unsafe fn visit_num(&mut self, num: &'ast Num);
    unsafe fn visit_str(&mut self, str: &'ast Str);
    unsafe fn visit_bool(&mut self, b: bool);
    unsafe fn visit_list(&mut self, list: &'ast List);
    unsafe fn visit_ident(&mut self, ident: &'ast Ident);
    unsafe fn visit_unary_op(&mut self, op: &'ast UnaryOp, expr: &'ast Expr);
    unsafe fn visit_binary_op(
        &mut self,
        op: &'ast BinaryOp,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        ty: &'ast Type,
    );
    unsafe fn visit_struct_access(&mut self, expr: &'ast Expr, ident: &'ast Ident);
    unsafe fn visit_lambda(&mut self, params: &'ast [Param], body: &'ast Expr);
    unsafe fn visit_block_expr(&mut self, bindings: &'ast [Binding], expr: &'ast Expr);
    unsafe fn visit_struct_expr(&mut self, fields: &'ast [Binding], ty: &'ast Type);
    unsafe fn visit_app(&mut self, lhs: &'ast Expr, rhs: &'ast Expr);
    unsafe fn visit_if_expr(
        &mut self,
        cond: &'ast Expr,
        then_expr: &'ast Expr,
        else_expr: &'ast Expr,
    );
    unsafe fn visit_bind(&mut self, bind: &'ast Binding);
    unsafe fn visit_struct_def(&mut self, struct_def: &'ast StructDef);
}
