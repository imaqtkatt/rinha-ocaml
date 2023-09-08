open Spec.Ast

type t =
  | Bool of bool
  | Int of int64
  | Str of string
  | Tup of t * t
  | Fn of closure * string list * term

and closure = t Env.Env.t

let rec string_of_obj = function
  | Bool b -> string_of_bool b
  | Int i -> Int64.to_string i
  | Str s -> s
  | Tup (l, r) -> "(" ^ string_of_obj l ^ " " ^ string_of_obj r ^ ")"
  | Fn (_, _, _) -> "<function>"
