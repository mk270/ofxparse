
open Ast


let header = function
  | Header (i, s) ->
     "header (" ^ string_of_ident i ^ ": " ^ s ^ ")"

let tag = function
  | Tag s     -> "Tag_(" ^ (string_of_ident s) ^ ")\n"
  | End_tag s -> "/Tag(" ^ (string_of_ident s) ^ ")"

let map_car f xx =
  let concat_with_comma x y = x ^ ", " ^ y in
    List.map f xx |> List.fold_left concat_with_comma ""

let rec registry = function
  | Headers hh -> map_car header hh

  | Cons (car, cdr) -> 
     let car' = registry car in
     let cdr' = registry cdr in
     "[" ^ car' ^ ", " ^ cdr' ^ " ]"

  | Eof -> "EOF"

  | Root_node n -> node n

and node_contents = function
  | Kvp (i, s) ->
     let i' = string_of_ident i in
       "(" ^ i' ^ " . " ^ s ^ ")"

  | Node n -> node n

and node node =
  let t = tag node.tag_name in
  let c = map_car node_contents node.node_contents in
    "Node(" ^ t ^ ": " ^ c ^ ")"
