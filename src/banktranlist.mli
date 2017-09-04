(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

open Ast

type t
type component

val parse_tuple : (ident * string) -> component
val make_transaction : Stmttrn.t -> component

val of_contents : component list -> t
val to_string : t -> string
val dump_start : t -> string
val dump_time_range : t -> string
val dump_transactions : t -> string list
