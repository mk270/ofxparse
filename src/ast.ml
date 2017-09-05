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

let ident s = Ident s
let header (i, s) = Header (i, s)
let tag i = Tag i
let end_tag i = End_tag i

let string_of_ident = function
	| Ident s -> s

let string_of_tag = function
  | Tag (Ident i) -> i
  | End_tag _ -> assert false

let string_of_node_tag_name n =
  string_of_tag n.tag_name

let tags_match t1 t2 =
  let string_of_tag = function
    | Tag i -> string_of_ident i
    | End_tag i -> string_of_ident i
  in
    string_of_tag t1 = string_of_tag t2


(* DUMP *)

(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

open Printf

module Dump = struct

let tuple (i, s) =
  let i' = string_of_ident i in
    sprintf "(%s . %s)" i' s

let header = function
  | Header h -> tuple h

let tag = function
  | Tag s     -> string_of_ident s |> sprintf "(tag %s)"
  | End_tag s -> string_of_ident s |> sprintf "(/tag %s)"

let map_car f xx =
  let concat_with_comma x y = x ^ ", " ^ y in
    List.map f xx |> List.fold_left concat_with_comma ""

let rec registry = function
  | Headers hh -> map_car header hh
  | Eof -> "EOF"
  | Root_node n -> node n

and node_contents = function
  | Kvp kvp -> tuple kvp
  | Node n -> node n

and node node =
  let t = tag node.tag_name in
  let c = map_car node_contents node.node_contents in
    sprintf "(node %s %s)" t c

end
