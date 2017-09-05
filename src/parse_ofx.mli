(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

val parse : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> bool -> Ast.t

