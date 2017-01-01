
type ident = Ident of string

type tag =
  | Tag of ident
  | End_tag of ident

type t =
  | Header of (ident * string)
  | Eof
  | Cons of (t * t)
  | Kvp of (ident * string)

let string_of_ident = function
	| Ident s -> "$" ^ s

