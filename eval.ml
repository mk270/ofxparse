
open Ast


let dump_header = function
  | Header (i, s) ->
     "header (" ^ string_of_ident i ^ ": " ^ s ^ ")"

let dump_tag = function
  | Tag s     -> "Tag_(" ^ (string_of_ident s) ^ ")\n"
  | End_tag s -> "/Tag(" ^ (string_of_ident s) ^ ")"

let rec dump_registry = function
  | Headers hh -> List.map dump_header hh |> List.fold_left (^) ""
  | Cons (car, cdr) -> 
     "cons (" ^ dump_registry car ^ ", " ^ dump_registry cdr ^ " )"
  | Eof -> "EOF"
  | Root_node n -> dump_node n
and dump_node_contents = function
  | Kvp (i, s) ->
     "KVP (" ^ string_of_ident i ^ ": " ^ s ^ ")"
  | Node n -> dump_node n
and dump_node node =
  "Node(" ^ (dump_tag node.tag_name) ^ ": " ^
    (node.node_contents
        |> List.map dump_node_contents
        |> List.fold_left (^) "")
  ^ ")"
