open Base
open Ast

val infer : Sub.env -> expr -> (Sub.sub * Ast.mono_type) option

