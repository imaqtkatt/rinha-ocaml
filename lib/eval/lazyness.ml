type 'a lazy_val =
  | Ready of 'a
  | Lazy of (unit -> 'a lazy_val)

let rec get = function
  | Ready v -> v
  | Lazy f -> get (f ())
