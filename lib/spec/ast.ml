type var = {
  text : string;
  location : loc;
}

and loc = Lexing.position * Lexing.position

type term =
  | If of {
      condition : term;
      then_ : term;
      otherwise : term;
      location : loc;
    }
  | Let of {
      name : var;
      value : term;
      next : term option;
      location : loc;
    }
  | Str of {
      value : string;
      location : loc;
    }
  | Bool of {
      value : bool;
      location : loc;
    }
  | Int of {
      value : int64;
      location : loc;
    }
  | Binary of {
      lhs : term;
      op : binary_op;
      rhs : term;
      location : loc;
    }
  | Call of {
      callee : string;
      arguments : term list;
      location : loc;
    }
  | Function of {
      parameters : var list;
      value : term;
      location : loc;
    }
  | Print of {
      value : term;
      location : loc;
    }
  | First of {
      value : term;
      location : loc;
    }
  | Second of {
      value : term;
      location : loc;
    }
  | Tuple of {
      first : term;
      second : term;
      location : loc;
    }
  | Var of var

and binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or

and file = {
  expression : term;
  location : loc;
}
