open Spec
open Eval.Env
open Eval.Evaluator

let src = "files/fib.rinha"

let handle_parse_result = function
  | Ok None -> print_endline "Nothing"
  | Ok (Some ({ expression; _ } : Ast.file)) ->
      let _ = eval Env.empty expression in
      ()
  | Error msg -> print_endline msg

let string_of_position (pos : Lexing.position) =
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let string_pos_of_lexbuf (lexbuf : Lexing.lexbuf) =
  let st = lexbuf.lex_start_p in
  let ed = lexbuf.lex_curr_p in
  Printf.sprintf "File: %s, Syntax error from %s to %s" st.pos_fname
    (string_of_position st) (string_of_position ed)

let () =
  let channel = In_channel.open_bin src in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = src };
  let result =
    try Ok (Parser.file Lexer.read_token lexbuf) with
    | Lexer.SyntaxError (e, pos) ->
        let msg = Printf.sprintf "%s at %s" e (string_of_position pos) in
        Error msg
    | Parser.Error ->
        let msg = string_pos_of_lexbuf lexbuf in
        Error msg
  in
  In_channel.close channel;
  handle_parse_result result
