
type ident = Ident of string

type tuple = (ident * string)

type tag =
  | Tag of ident
  | End_tag of ident

type header =
  | Header of tuple

type t =
  | Headers of header list
  | Eof
  | Cons of (t * t)
  | Kvp of tuple

let string_of_ident = function
	| Ident s -> "$" ^ s

