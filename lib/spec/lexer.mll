{
  open Parser
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
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPARENS }
  | ')' { RPARENS }
  | '=' { EQ }
  | ';' { SEMICOLON }
  | ',' { COMMA }
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
  | identifier as id { IDENTIFIER id }
  | identifier alphanumeric * as id { IDENTIFIER id }
  | _ { EOF }
  | eof { EOF }
