
{
  open Parser
  open Lexing

  exception Unexpected_token

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

}
let digit = ['0'-'9']
let letter = ['a'-'z']
let ucletter = ['A'-'Z']
let alnum = (letter | digit)
let ucalnum = (ucletter | digit)
let identifier = ucletter ucalnum*
let nonquote = [ ^ '"' ]
let nonnl = [ ^ '\n' ]
let nonnlorcolon = [ ^ '\n' ':' ]
let nonangle = [ ^ '<' '>' ]
let whitespace = [ ' ' '\t' '\n' ]

rule token = parse
  | '\n'            { incr_lineno lexbuf; token lexbuf }
  | "</"            { identifier_token false lexbuf }
  | '<'             { identifier_token true lexbuf }
  | '>'             { GT } 
  | whitespace+     { token lexbuf }
  | nonangle+ as cd { CDATA cd }
  | _               { raise Unexpected_token }
  | eof		        { EOF }
and identifier_token opening = parse
  | identifier as id   { 
    if opening
    then TAG id
    else ENDTAG id
  }

and header_token = parse
  | [' ' '\t']	       { header_token lexbuf }
  | "\n\n"             { EOF }
  | '\n'		       { incr_lineno lexbuf; header_token lexbuf }
  | ':'                { header_value lexbuf }
  | identifier as id   { IDENTIFIER id }
  | _ { raise Unexpected_token }
  | eof		{ EOF }

and header_value = parse
  | nonnlorcolon + as cd { HEADER_DATA cd }
