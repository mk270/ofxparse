
type ident = Ident of string

type t =
  | Header of (ident * string)
  | Tag of ident
  | End_tag of ident
  | Eof
  | Cons of (t * t)
  | Kvp of (ident * string)

let string_of_ident = function
	| Ident s -> "$" ^ s

