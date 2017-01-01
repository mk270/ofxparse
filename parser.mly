
%{

open Ast

let parse_error s = Printf.printf "Parse error: %s\n" s


%}

/* Ocamlyacc Declarations */
%token NEWLINE
%token LPAREN RPAREN
%token <Big_int.big_int> NUM
%token <string> STRING
%token LBRACE RBRACE
%token <string> IDENTIFIER
%token <string> TAG
%token <string> ENDTAG
%token EOF
%token COMMA
%token COLON
%token LT GT
%token SLASH
%token <string> CDATA

%left PLUS MINUS
%left MULTIPLY
%left COMMA
%right CARET	/* exponentiation */

%start input
%type <Ast.t> input

/* Grammar follows */
%%
input:
   | EOF { Eof } 
   | header input { Cons ($1, $2) }
   | node   input { Cons ($1, $2) }
;
header:
   | ident COLON num { Header ($1, $3) }
   | ident COLON ident { Header ($1, "$3") }
   | ident COLON cdata_header { Header ($1, $3) }
;
num:
  | NUM { "NuM" }
;
ident:
  | IDENTIFIER { Ident $1 }
;
node:
  | open_tag node_contents close_tag { $2 }
;
tag:
  | open_tag  { print_endline "<tag>"; $1 }
  | close_tag { print_endline "</tag>"; $1 }
;
open_tag:
  | TAG GT    { Tag     (Ident $1) }
;
close_tag:
  | ENDTAG GT { End_tag (Ident $1) }
;
cdata_header:
  | CDATA { $1 }
;
cdata:
  | CDATA { CData $1 }
;
unclosed_node:
  | TAG GT CDATA { Kvp (Ident $1, $3) }
;

node_contents:
  | node_content node_contents { Cons ($1, $2) }
  | node_content { $1 }
;
node_content:
  | node { $1 }
  | unclosed_node { $1 }
;
