open Base

type sub = (int * Ast.mono_type) list
type env = Ast.poly_type Map.M(Int).t

val find : sub -> Ast.type_var_name -> Ast.mono_type option
val find_exn : sub -> Ast.type_var_name -> Ast.mono_type
val mem : sub -> Ast.type_var_name -> bool
val map : sub -> (Ast.mono_type -> Ast.mono_type) -> sub
val filter : sub -> (Ast.type_var_name -> bool) -> sub
val apply_mono : sub -> Ast.mono_type -> Ast.mono_type
val apply_poly : sub -> Ast.poly_type -> Ast.poly_type
val apply_over_env : sub -> env -> env
