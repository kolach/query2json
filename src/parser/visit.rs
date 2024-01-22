use crate::parser::syn::{
    Expr, ExprBool, ExprInArray, ExprInRange, ExprLogic, ExprReg, ExprRel, ExprValue,
};

pub trait Visit<'ast> {
    type Result;

    fn visit_expr_value(&mut self, _node: &'ast ExprValue) -> Self::Result;
    fn visit_expr_bool(&mut self, _node: &'ast ExprBool) -> Self::Result;
    fn visit_expr_rel(&mut self, _node: &'ast ExprRel) -> Self::Result;
    fn visit_expr_in_array(&mut self, _node: &'ast ExprInArray) -> Self::Result;
    fn visit_expr_in_range(&mut self, _node: &'ast ExprInRange) -> Self::Result;
    fn visit_expr_reg(&mut self, _node: &'ast ExprReg) -> Self::Result;
    fn visit_expr_logic(&mut self, _node: &'ast ExprLogic) -> Self::Result;
    fn visit_expr_empty(&mut self) -> Self::Result;
}

pub trait VisitMut<'ast> {
    type Result;

    fn visit_expr_value_mut(&mut self, _node: &'ast mut ExprValue<'ast>) -> Self::Result;
    fn visit_expr_bool_mut(&mut self, _node: &'ast mut ExprBool<'ast>) -> Self::Result;
    fn visit_expr_rel_mut(&mut self, _node: &'ast mut ExprRel<'ast>) -> Self::Result;
    fn visit_expr_in_array_mut(&mut self, _node: &'ast mut ExprInArray<'ast>) -> Self::Result;
    fn visit_expr_in_range_mut(&mut self, _node: &'ast mut ExprInRange<'ast>) -> Self::Result;
    fn visit_expr_reg_mut(&mut self, _node: &'ast mut ExprReg<'ast>) -> Self::Result;
    fn visit_expr_logic_mut(&mut self, _node: &'ast mut ExprLogic<'ast>) -> Self::Result;
    fn visit_expr_empty_mut(&mut self) -> Self::Result;
}

pub fn visit<'ast, V: Visit<'ast>>(node: &'ast Expr<'ast>, visit: &mut V) -> V::Result {
    match node {
        Expr::Value(v) => visit.visit_expr_value(v),
        Expr::Bool(v) => visit.visit_expr_bool(v),
        Expr::Rel(v) => visit.visit_expr_rel(v),
        Expr::InArray(v) => visit.visit_expr_in_array(v),
        Expr::InRange(v) => visit.visit_expr_in_range(v),
        Expr::Reg(v) => visit.visit_expr_reg(v),
        Expr::Logic(v) => visit.visit_expr_logic(v),
        Expr::Empty => visit.visit_expr_empty(),
    }
}

pub fn visit_mut<'ast, V: VisitMut<'ast>>(node: &'ast mut Expr<'ast>, visit: &mut V) -> V::Result {
    match node {
        Expr::Value(v) => visit.visit_expr_value_mut(v),
        Expr::Bool(v) => visit.visit_expr_bool_mut(v),
        Expr::Rel(v) => visit.visit_expr_rel_mut(v),
        Expr::InArray(v) => visit.visit_expr_in_array_mut(v),
        Expr::InRange(v) => visit.visit_expr_in_range_mut(v),
        Expr::Reg(v) => visit.visit_expr_reg_mut(v),
        Expr::Logic(v) => visit.visit_expr_logic_mut(v),
        Expr::Empty => visit.visit_expr_empty_mut(),
    }
}

