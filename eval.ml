
open Ast


let dump_header = function
  | Header (i, s) ->
     "header (" ^ string_of_ident i ^ ": " ^ s ^ ")"

let dump_tag = function
  | Tag s     -> "Tag_(" ^ (string_of_ident s) ^ ")\n"
  | End_tag s -> "/Tag(" ^ (string_of_ident s) ^ ")"

let map_car f xx =
  let concat_with_comma x y = x ^ ", " ^ y in
    List.map f xx |> List.fold_left concat_with_comma ""

let rec dump_registry = function
  | Headers hh -> map_car dump_header hh

  | Cons (car, cdr) -> 
     let car' = dump_registry car in
     let cdr' = dump_registry cdr in
     "[" ^ car' ^ ", " ^ cdr' ^ " ]"

  | Eof -> "EOF"

  | Root_node n -> dump_node n

and dump_node_contents = function
  | Kvp (i, s) ->
     let i' = string_of_ident i in
       "(" ^ i' ^ " . " ^ s ^ ")"

  | Node n -> dump_node n

and dump_node node =
  let t = dump_tag node.tag_name in
  let c = map_car dump_node_contents node.node_contents in
    "Node(" ^ t ^ ": " ^ c ^ ")"
