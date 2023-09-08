open Env
open Spec.Ast

exception EvalException of string

type eval_memo = ((string * Object.t) list, Object.t) Hashtbl.t

let (memo : eval_memo) = Hashtbl.create 50

let rec eval env = function
  | Int { value; _ } -> Object.Int value
  | Bool { value; _ } -> Object.Bool value
  | Str { value; _ } -> Object.Str value
  | Tuple { first; second; _ } ->
      Object.Tup (eval env first, eval env second)
  | First { value; _ } -> (
      match eval env value with
      | Object.Tup (v, _) -> v
      | _ -> assert false)
  | Second { value; _ } -> (
      match eval env value with
      | Object.Tup (_, v) -> v
      | _ -> assert false)
  | Var { text; _ } -> (
      match Env.find_opt text env with
      | Some o -> o
      | None -> raise @@ EvalException ("Unbound variable " ^ text))
  | Let { name; value; next; _ } -> (
      let v = eval env value in
      let env' = Env.add name.text v env in
      match next with
      | Some n -> eval env' n
      | None -> v)
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
      let f = Env.find callee env in
      match f with
      | Object.Fn (closure, args, body) as f -> (
          let al = List.map (fun a -> eval env a) arguments in
          let x = List.combine args al in
          let x = x @ [ (callee, f) ] in
          match Hashtbl.find_opt memo x with
          | Some o -> o
          | None ->
              let closure' = Env.add_seq (List.to_seq x) closure in
              let ret = eval closure' body in
              let _ = Hashtbl.add memo x ret in
              ret)
      | _ -> assert false)
  | Binary { lhs; op; rhs; _ } -> eval_binary (eval env lhs) (eval env rhs) op

and eval_binary lhs rhs op =
  let open Object in
  let v =
    match (lhs, rhs, op) with
    | Int l, Int r, Add -> Int (Int64.add l r)
    | Int l, Str r, Add -> Str (Int64.to_string l ^ r)
    | Str l, Int r, Add -> Str (l ^ Int64.to_string r)
    | Int l, Int r, Sub -> Int (Int64.sub l r)
    | Int l, Int r, Mul -> Int (Int64.mul l r)
    | Int l, Int r, Div -> Int (Int64.div l r)
    | Int l, Int r, Eq -> Bool (Int64.equal l r)
    | Int l, Int r, Lt -> Bool (l < r)
    | Bool l, Bool r, Or -> Bool (Bool.( || ) l r)
    | l, r, _ ->
        print_endline @@ string_of_obj l;
        print_endline @@ string_of_obj r;
        assert false
  in
  v
