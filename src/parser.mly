
%{

open Ast

let parse_error s = Printf.printf "Parse error: %s\n" s

let tags_match t1 t2 =
  let string_of_tag = function
    | Tag i -> string_of_ident i
    | End_tag i -> string_of_ident i
  in
    string_of_tag t1 = string_of_tag t2

%}

/* Ocamlyacc Declarations */
%token NEWLINE
%token <string> IDENTIFIER
%token <string> TAG
%token <string> ENDTAG
%token EOF
%token LT GT
%token SLASH
%token <string> CDATA
%token <string> HEADER_DATA

%start input
%type <Ast.t> input

/* Grammar follows */
%%
input:
   | EOF { Eof } 
   | headers input { Headers $1 }
   | node   input { Root_node $1 }
;
headers:
   | header { [ $1 ] }
   | header headers { $1 :: $2 }
;
header:
   | ident cdata_header { Header ($1, $2) }
;
ident:
   | IDENTIFIER { Ident $1 }
;
node:
   | open_tag node_contents close_tag { 
     assert (tags_match $1 $3);
     { tag_name = $1; node_contents = $2 }
   }
;
open_tag:
   | TAG GT    { Tag     (Ident $1) }
;
close_tag:
   | ENDTAG GT { End_tag (Ident $1) }
;
cdata_header:
   | HEADER_DATA { $1 }
;
unclosed_node:
   | TAG GT CDATA { Kvp (Ident $1, $3) }
;

node_contents:
   | node_content node_contents { $1 :: $2 }
   | node_content { [ $1 ] }
;
node_content:
   | node { Node $1 }
   | unclosed_node { $1 }
;
