
open Ast

type t
type component

val parse_tuple : (ident * string) -> component
val make_transaction : Stmttrn.t -> component

val of_contents : component list -> t
val to_string : t -> string
val dump_start : t -> string
val dump_time_range : t -> string
val dump_transactions : t -> string
