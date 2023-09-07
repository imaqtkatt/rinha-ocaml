%{
  open Ast
%}

%token <string> IDENTIFIER
%token <int32> INT
%token <string> STRING
%token TRUE
%token FALSE

%token IF
%token ELSE
%token LET
%token FN
%token FAT_ARROW
%token PRINT
%token FIRST
%token SECOND

%token LPARENS
%token RPARENS
%token LBRACE
%token RBRACE

%token EQ
%token PLUS
%token MIN
%token MUL
%token DIV
%token LT

%token SEMICOLON
%token COMMA

%token EOF

%start <Ast.file option> file

%type <Ast.term> term

%type <Ast.var> var

%type <Ast.binary_op> op

(** TODO: make this right *)
%left PLUS MIN
%left MUL DIV
%left LT

%%

file:
  | EOF; { None }
  | t = term; EOF;
    { Some { name = ""; expression = t; location = $loc; } }

(** Terms *)
let term :=
  | infix
  | let_expr
  | fn
  | if_expr
  | two_tuple
  | builtin

(** Tuple *)
let two_tuple :=
  | LPARENS; f = term; COMMA; s = term; RPARENS;
    { Tuple { first = f; second = s; location = $loc; } }

(** If expr *)
let if_expr :=
  IF; c = term; LBRACE; t = term; RBRACE; ELSE; LBRACE; o = term; RBRACE;
    { If { condition = c; then_ = t; otherwise = o; location = $loc; } }

(** Functions *)
let block ==
  | term
  | LBRACE; t = term; RBRACE; { t }

let fn :=
  | FN; LPARENS; ps = separated_list(COMMA, var); RPARENS; FAT_ARROW; t = block;
    { Function { parameters = ps; value = t; location = $loc; } }

(** Let expr *)
let next_let ==
  | EOF; { None }
  | SEMICOLON; n = term; { Some n }

let let_expr :=
  | LET; v = var; EQ; t = term; n = next_let;
    { Let { name = v; value = t; next = n; location = $loc; } }

(** Infix *)
%inline op:
  | PLUS; { Add }
  | MIN; { Sub }
  | MUL; { Mul }
  | DIV; { Div }
  | LT; { Lt }

let infix :=
  | call
  | l = infix; op = op; r = infix;
    { Binary { lhs = l; op = op; rhs = r; location = $loc; } }

(** Calls *)
%inline arg:
  | LPARENS; t = term; RPARENS; { t }

let print ==
  | PRINT; a = arg; { Print { value = a; location = $loc; } }

let first ==
  | FIRST; a = arg; { First { value = a; location = $loc; } }

let second ==
  | SECOND; a = arg; { Second { value = a; location = $loc; } }

let builtin ==
  | print
  | first
  | second

let call :=
  | sub_term
  | i = IDENTIFIER; LPARENS; args = separated_list(COMMA, term); RPARENS;
    { Call { callee = i; arguments = args; location = $loc; } }

(** Literals *)
let sub_term :=
  | literal
  | LPARENS; t = term; RPARENS; { t }

let literal :=
  | i = INT; { Int { value = i; location = $loc; } }
  | s = STRING; { Str { value = s; location = $loc; } }
  | TRUE; { Bool { value = true; location = $loc; } }
  | FALSE; { Bool { value = false; location = $loc; } }
  | v = var; { Var v }

let var :=
  | i = IDENTIFIER; { { text = i; location = $loc; } }
