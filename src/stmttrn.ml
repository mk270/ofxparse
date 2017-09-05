(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type t = {
  trntype : Trntype.t;
  dtposted : Timestamp.t;
  trnamt : Currency.t;
  fitid : string;
  name : string;
}

exception Key_not_found of string * string

let get_string' expected_key = function
  | (ident, s) -> let id = Ast.string_of_ident ident
                  in
                    if id = expected_key
                    then s
                    else raise (Key_not_found (id, expected_key))

let get_string expected_key kvp =
  get_string' expected_key kvp |> String.trim

let get_name kvp    = get_string "NAME" kvp
let get_fitid kvp   = get_string "FITID" kvp
let get_trnamt kvp  = get_string "TRNAMT" kvp
let get_trntype kvp = get_string "TRNTYPE" kvp |> Trntype.trntype_of_string
let get_date kvp    = get_string "DTPOSTED" kvp |> Timestamp.of_string

let of_tuples a b c d e =
    {
      trntype = get_trntype a;
      dtposted = get_date b;
      trnamt = get_trnamt c |> Currency.of_string;
      fitid = get_fitid d;
      name = get_name e;
    }

let string_of_stmttrn trn =
  List.fold_left (^) "" [
    "([";
    trn.fitid;
    "] ";
    Timestamp.to_string trn.dtposted;
    " ";
    Trntype.string_of_trntype trn.trntype;
    ": ";
    Currency.to_string trn.trnamt;
    " :: ";
    trn.name;
    ")"
  ]
