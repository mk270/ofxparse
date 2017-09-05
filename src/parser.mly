%{

(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

open Ast

let parse_error s = Printf.printf "Parse error: %s\n" s

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
   | ident cdata_header { header ($1, $2) }
;
ident:
   | IDENTIFIER { ident $1 }
;
node:
   | open_tag node_contents close_tag { 
     assert (tags_match $1 $3);
     { tag_name = $1; node_contents = $2 }
   }
;
open_tag:
   | TAG GT    { tag     (ident $1) }
;
close_tag:
   | ENDTAG GT { end_tag (ident $1) }
;
cdata_header:
   | HEADER_DATA { $1 }
;
unclosed_node:
   | TAG GT CDATA { Kvp (ident $1, $3) }
;

node_contents:
   | node_content node_contents { $1 :: $2 }
   | node_content { [ $1 ] }
;
node_content:
   | node { Node $1 }
   | unclosed_node { $1 }
;
