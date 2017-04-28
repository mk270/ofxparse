(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type ident = Ident of string

type tuple = (ident * string)

type tag =
  | Tag of ident
  | End_tag of ident

type header =
  | Header of tuple

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

let string_of_ident = function
	| Ident s -> s

let string_of_tag = function
  | Tag (Ident i) -> i
  | End_tag _ -> assert false
