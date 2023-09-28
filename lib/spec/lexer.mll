{
  open Parser
  exception SyntaxError of string * Lexing.position

  let unexpected_char lexbuf =
    SyntaxError (
      "Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'",
      Lexing.lexeme_start_p lexbuf
    )

  (* let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
        pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1;
      } *)
}

let digit = ['0'-'9']
let int = '-'? digit+

let identifier = ['a'-'z' 'A'-'Z' '_']+
let alphanumeric = ['0'-'9' 'a'-'z' 'A'-'Z' '_']+

let newline = ['\n']
let skippable = [' ' '\t']

rule read_token = parse
  | newline { Lexing.new_line lexbuf; read_token lexbuf }
  | skippable+ { read_token lexbuf }
  | "//" { read_comment lexbuf }
  | "/*" { read_multi_comment 0 lexbuf }
  | '+' { PLUS }
  | '-' { MIN }
  | '*' { MUL }
  | '/' { DIV }
  | '<' { LT }
  | '>' { GT }
  | '=' { EQ }
  | '=' '=' { EQL }
  | '!' '=' { NEQ }
  | '<' '=' { LTE }
  | '>' '=' { GTE }
  | '&' '&' { AND }
  | '|' '|' { OR }
  | '%' { REM }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPARENS }
  | ')' { RPARENS }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "print" { PRINT }
  | "first" { FIRST }
  | "second" { SECOND }
  | "fn" { FN }
  | "=>" { FAT_ARROW }
  | "let" { LET }
  | "if" { IF }
  | "else" { ELSE }
  | int as i { INT (Int32.of_string i) }
  | identifier alphanumeric * as id { IDENTIFIER id }
  | _ { raise @@ unexpected_char lexbuf }
  | eof { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof { raise @@ SyntaxError
    ("Unfinished string", Lexing.lexeme_end_p lexbuf) }
  | _ { raise @@ unexpected_char lexbuf }

and read_comment = parse
  | newline { Lexing.new_line lexbuf; read_token lexbuf }
  | eof { raise @@ SyntaxError ("Unfinished comment", Lexing.lexeme_end_p lexbuf) }
  | _ { read_comment lexbuf }

and read_multi_comment depth = parse
  | "*/" { if depth = 0 then read_token lexbuf else read_multi_comment (depth - 1) lexbuf }
  | "/*" { read_multi_comment (depth + 1) lexbuf }
  | eof { raise @@ SyntaxError ("Unfinished comment", Lexing.lexeme_end_p lexbuf) }
  | _ { read_multi_comment depth lexbuf }
