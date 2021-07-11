open Expr
open Typed_expr

val infer_type : expr -> (expr_type, Type_unify.typing_error) Result.t
