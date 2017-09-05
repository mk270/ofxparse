(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

(* TODO: make more of these types abstract *)

type ident

type tuple = (ident * string)

type header

type tag

type node = {
  tag_name : tag;
  node_contents : node_contents list
} 
and node_contents =
  | Kvp of tuple
  | Node of node

type t =
  | Headers of header list
  | Eof
  | Root_node of node

val ident : string -> ident
val header : ident * string -> header
val tag : ident -> tag
val end_tag : ident -> tag

val string_of_ident : ident -> string
val string_of_tag : tag -> string
val string_of_node_tag_name : node -> string

val tags_match : tag -> tag -> bool

module Dump : sig
  val tuple : tuple -> string

end
