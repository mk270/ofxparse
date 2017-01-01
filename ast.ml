
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
  | Root_node of node
and node = {
  tag_name : tag;
  node_contents : node_contents list
} 
and node_contents =
  | Kvp of tuple
  | Node of node

let string_of_ident = function
	| Ident s -> s

