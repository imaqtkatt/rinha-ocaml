open Env
open Spec.Ast

exception EvalException of string

type eval_memo = (Object.t Env.t, Object.t) Hashtbl.t

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
  | Tuple { first; second; _ } -> eval_tup env first second
  | First { value; _ } -> eval_fst value env eval
  | Second { value; _ } -> eval_snd value env eval
  | Let { name = { text; _ }; value; next; _ } -> (
      match eval env value with
      | Object.Fn _ as f ->
          let env' =
            if String.starts_with ~prefix:"_" text then env
            else (
              fn_decls := Env.add text f !fn_decls;
              Env.add text f env)
          in
          eval env' next
      | _ as v ->
          let env' =
            if String.starts_with ~prefix:"_" text then env
            else Env.add text v env
          in
          eval env' next)
  | Function { parameters; value; _ } -> eval_fn value env parameters
  | Print { value; _ } -> eval_print value env eval
  | If { condition; then_; otherwise; _ } ->
      eval_if env condition then_ otherwise
  | Call { callee; arguments; _ } -> (
      match find_fun callee with
      | Object.Fn (closure, args, body) -> (
          let closure' =
            List.combine args arguments
            |> List.map (function s, term -> (s, eval env term))
            |> List.fold_left (fun env (s, obj) -> Env.add s obj env) closure
          in
          match Hashtbl.find_opt memo closure' with
          | Some o -> o
          | None ->
              let ret = eval closure' body in
              Hashtbl.add memo closure' ret;
              ret)
      | _ -> assert false)
  | Binary { lhs; op; rhs; _ } -> eval_binary (eval env lhs) (eval env rhs) op

and eval_tup env f s = Object.Tup (eval env f, eval env s)

and eval_fst value env eval =
  match eval env value with
  | Object.Tup (v, _) -> v
  | _ -> assert false

and eval_snd value env eval =
  match eval env value with
  | Object.Tup (_, v) -> v
  | _ -> assert false

and eval_fn value env parameters =
  let ps = List.map (fun p -> p.text) parameters in
  Object.Fn (env, ps, value)

and eval_print value env eval =
  let o = eval env value in
  let () = print_endline @@ Object.string_of_obj @@ o in
  o

and eval_if env condition then_ otherwise =
  match eval env condition with
  | Object.Bool b -> if b then eval env then_ else eval env otherwise
  | _ -> eval env then_

and eval_binary lhs rhs op =
  let open Object in
  match (op, lhs, rhs) with
  | Add, Int l, Int r -> Int (Int64.add l r)
  | Add, Int l, Str r -> Str (Int64.to_string l ^ r)
  | Add, Str l, Int r -> Str (l ^ Int64.to_string r)
  | Sub, Int l, Int r -> Int (Int64.sub l r)
  | Mul, Int l, Int r -> Int (Int64.mul l r)
  | Div, Int l, Int r -> Int (Int64.div l r)
  | Eq, Int l, Int r -> Bool (Int64.equal l r)
  | Eq, Bool l, Bool r -> Bool (Bool.equal l r)
  | Eq, Str l, Str r -> Bool (String.equal l r)
  | Lte, Int l, Int r -> Bool (l <= r)
  | Lt, Int l, Int r -> Bool (l < r)
  | Gte, Int l, Int r -> Bool (l >= r)
  | Gt, Int l, Int r -> Bool (l > r)
  | Or, Bool l, Bool r -> Bool (Bool.( || ) l r)
  | And, Bool l, Bool r -> Bool (Bool.( && ) l r)
  | _, l, r ->
      print_endline @@ string_of_obj l;
      print_endline @@ string_of_obj r;
      assert false
