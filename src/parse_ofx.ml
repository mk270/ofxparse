(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

open Ast

let report_lex lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    let lnum = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum in
    let lexeme = Lexing.lexeme lexbuf in
    Printf.printf "line: %d, byte: %d, `%s'\n"
        lnum cnum lexeme

let lexbuf = Lexing.from_channel stdin

let parse lx debug =
    let _ = Parsing.set_trace debug in

    try Parser.input lx lexbuf
    with
    | Failure f ->
        report_lex lexbuf;
        print_endline f;
        failwith f
    | Parsing.Parse_error ->
        report_lex lexbuf;
        failwith "parse error"
    | Lexer.Unexpected_token ->
        report_lex lexbuf;
        failwith "lexer error"
