open Env
open Spec.Ast

exception EvalException of string

type eval_memo = ((string * Object.t) list, Object.t) Hashtbl.t

let (memo : eval_memo) = Hashtbl.create 50
let fn_decls = ref Env.empty

let find_fun = function
  | Var { text; _ } -> Env.find text !fn_decls
  | _ -> assert false

let rec eval env t =
  match t with
  | Var { text; _ } -> Env.find text env
  | Int { value; _ } -> Object.Int value
  | Bool { value; _ } -> Object.Bool value
  | Str { value; _ } -> Object.Str value
  | Tuple { first; second; _ } -> Object.Tup (eval env first, eval env second)
  | First { value; _ } -> (
      match eval env value with
      | Object.Tup (v, _) -> v
      | _ -> assert false)
  | Second { value; _ } -> (
      match eval env value with
      | Object.Tup (_, v) -> v
      | _ -> assert false)
  | Let { name = { text; _ }; value; next; _ } -> (
      let v = eval env value in
      match v with
      | Object.Fn _ as f ->
          fn_decls := Env.add text f !fn_decls;
          let env' = Env.add text f env in
          eval env' next
      | _ ->
          let env' = Env.add text v env in
          eval env' next)
  | Function { parameters; value; _ } ->
      let ps = List.map (fun p -> p.text) parameters in
      Object.Fn (env, ps, value)
  | Print { value; _ } ->
      let o = eval env value in
      let () = print_endline @@ Object.string_of_obj @@ o in
      o
  | If { condition; then_; otherwise; _ } -> (
      match eval env condition with
      | Object.Bool b -> if b then eval env then_ else eval env otherwise
      | _ -> eval env then_)
  | Call { callee; arguments; _ } -> (
      match find_fun callee with
      | Object.Fn (closure, args, body) ->
          let x = List.combine args arguments in
          let argument_list =
            List.map
              (function
                | s, t -> (s, eval env t))
              x
          in
          let closure' =
            List.fold_left (fun m (k, v) -> Env.add k v m) closure argument_list
          in
          eval closure' body
      | _ -> assert false)
  | Binary { lhs; op; rhs; _ } -> eval_binary (eval env lhs) (eval env rhs) op

and eval_binary lhs rhs op =
  let open Object in
  match (lhs, rhs, op) with
  | Int l, Int r, Add -> Int (Int64.add l r)
  | Int l, Str r, Add -> Str (Int64.to_string l ^ r)
  | Str l, Int r, Add -> Str (l ^ Int64.to_string r)
  | Int l, Int r, Sub -> Int (Int64.sub l r)
  | Int l, Int r, Mul -> Int (Int64.mul l r)
  | Int l, Int r, Div -> Int (Int64.div l r)
  | Int l, Int r, Eq -> Bool (Int64.equal l r)
  | Int l, Int r, Lte -> Bool (l <= r)
  | Int l, Int r, Lt -> Bool (l < r)
  | Bool l, Bool r, Or -> Bool (Bool.( || ) l r)
  | l, r, _ ->
      print_endline @@ string_of_obj l;
      print_endline @@ string_of_obj r;
      assert false
