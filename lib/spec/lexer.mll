{
  open Parser
  exception SyntaxError of string * Lexing.position

  let unexpected_char lexbuf =
    SyntaxError (
      "Unexpected character '" ^ Lexing.lexeme lexbuf ^ "'",
      Lexing.lexeme_start_p lexbuf
    )
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
  | int as i { INT (Int64.of_string i) }
  | identifier alphanumeric * as id { IDENTIFIER id }
  | _ { raise @@ unexpected_char lexbuf }
  | eof { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof { raise @@ SyntaxError
    ("Unfinished string", Lexing.lexeme_end_p lexbuf) }
  | _ { raise @@ unexpected_char lexbuf }
