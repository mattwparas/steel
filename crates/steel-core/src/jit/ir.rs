use std::collections::HashMap;

/// The AST node for expressions.
#[derive(Clone, Debug)]
pub enum Expr {
    Literal(String),
    Identifier(String),
    Assign(String, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    WhileLoop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    GlobalDataAddr(String),
    Block(Vec<Expr>),
}

/// The type representing JIT level operations
/// Given an input type
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum JitType {
    Float,
    Int,
    List,
    Bool,
    Void,
    Any,
}

#[derive(Clone, Debug)]
pub struct JitFunctionSignature {
    arguments: Vec<JitType>,
    return_value: JitType,
}

impl JitFunctionSignature {
    pub fn new(arguments: Vec<JitType>, return_value: JitType) -> Self {
        Self {
            arguments,
            return_value,
        }
    }
}

pub enum TypedExpr {
    Literal(String, JitType),
    Identifier(String),
    Equal(Box<TypedExpr>, Box<TypedExpr>),
    Add(Box<TypedExpr>, Box<TypedExpr>),
    Sub(Box<TypedExpr>, Box<TypedExpr>),
    Mul(Box<TypedExpr>, Box<TypedExpr>),
    LtE(Box<TypedExpr>, Box<TypedExpr>),
    IfElse(Box<TypedExpr>, Vec<TypedExpr>, Vec<TypedExpr>),
    Call(String, Vec<TypedExpr>),
}

pub fn typecheck(
    expr: &TypedExpr,
    bindings: &mut HashMap<String, JitType>,
    functions: &HashMap<String, JitFunctionSignature>,
) -> JitType {
    match expr {
        TypedExpr::Literal(_, ty) => *ty,
        TypedExpr::Identifier(name) => bindings
            .get(name)
            .copied()
            .expect(format!("Free Identifier: {}", name).as_str()),
        TypedExpr::Equal(_, _) | TypedExpr::LtE(_, _) => JitType::Bool,
        TypedExpr::Add(left, right) | TypedExpr::Sub(left, right) | TypedExpr::Mul(left, right) => {
            let left_type = typecheck(left, bindings, functions);
            let right_type = typecheck(right, bindings, functions);
            if let (JitType::Int, JitType::Int) = (left_type, right_type) {
                JitType::Int
            } else {
                JitType::Any
            }
        }
        TypedExpr::IfElse(_, then_expr, else_expr) => {
            let then_type = then_expr
                .iter()
                .map(|then_expr| typecheck(then_expr, bindings, functions))
                .last()
                .expect("Empty then expression");
            let else_type = else_expr
                .iter()
                .map(|else_expr| typecheck(else_expr, bindings, functions))
                .last()
                .expect("Empty else expression");
            if then_type == else_type {
                then_type
            } else {
                JitType::Any
            }
        }
        TypedExpr::Call(func, args) => {
            let function_signature = functions
                .get(func)
                .expect(format!("Free Identifier: {}", func).as_str());

            // Type check arguments
            for (input_arg, func_arg_type) in args.iter().zip(&function_signature.arguments) {
                let input_arg_type = typecheck(input_arg, bindings, functions);
                if input_arg_type != *func_arg_type {
                    return JitType::Any;
                }
            }

            // If we made it to this point, we successfully type checked the function call and can
            // Assume that the inputs are correct
            function_signature.return_value
        }
    }
}

#[cfg(test)]
mod type_checking {

    use super::*;

    #[test]
    fn fib_test() {
        let fib_test = TypedExpr::IfElse(
            Box::new(TypedExpr::LtE(
                Box::new(TypedExpr::Identifier("n".to_string())),
                Box::new(TypedExpr::Literal("2".to_string(), JitType::Int)),
            )),
            vec![TypedExpr::Literal("1".to_string(), JitType::Int)],
            vec![TypedExpr::Add(
                Box::new(TypedExpr::Call(
                    "fib".to_string(),
                    vec![TypedExpr::Sub(
                        Box::new(TypedExpr::Identifier("n".to_string())),
                        Box::new(TypedExpr::Literal("1".to_string(), JitType::Int)),
                    )],
                )),
                Box::new(TypedExpr::Call(
                    "fib".to_string(),
                    vec![TypedExpr::Sub(
                        Box::new(TypedExpr::Identifier("n".to_string())),
                        Box::new(TypedExpr::Literal("2".to_string(), JitType::Int)),
                    )],
                )),
            )],
        );

        let mut bindings = HashMap::new();
        bindings.insert("n".to_string(), JitType::Int);

        let mut functions = HashMap::new();
        functions.insert(
            "fib".to_string(),
            JitFunctionSignature::new(vec![JitType::Int], JitType::Int),
        );

        let output = typecheck(&fib_test, &mut bindings, &functions);

        assert_eq!(output, JitType::Int)
    }
}
