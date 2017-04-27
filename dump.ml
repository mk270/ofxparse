
open Ast
open Printf

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
