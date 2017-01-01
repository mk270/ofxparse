
open Ast

let rec dump_registry = function
  | Header (i, s) ->
     "header (" ^ string_of_ident i ^ ": " ^ s ^ ")"
  | Cons (car, cdr) -> 
     "cons (" ^ dump_registry car ^ ", " ^ dump_registry cdr ^ " )"
  | Eof -> "EOF"
  | Tag s     -> "Tag_(" ^ (string_of_ident s) ^ ")\n"
  | End_tag s -> "/Tag(" ^ (string_of_ident s) ^ ")"
  | Kvp (i, s) ->
     "KVP (" ^ string_of_ident i ^ ": " ^ s ^ ")"
