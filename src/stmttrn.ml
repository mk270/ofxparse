(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type trntype =
  | DIRECTDEBIT
  | PAYMENT
  | OTHER
  | REPEATPMT
  | DIRECTDEP
  | CASH
  | MATCHFAILURE

type t = {
  trntype : trntype;
  dtposted : Timestamp.t;
  trnamt : Currency.t;
  fitid : string;
  name : string;
}

exception Key_not_found of string * string

let trntype_of_string = function
  | "DIRECTDEBIT" -> DIRECTDEBIT
  | "PAYMENT" -> PAYMENT
  | "OTHER" -> OTHER
  | "REPEATPMT" -> REPEATPMT
  | "DIRECTDEP" -> DIRECTDEP
  | "CASH" -> CASH
  | _ -> MATCHFAILURE

let string_of_trntype = function
  | DIRECTDEBIT -> "DIRECTDEBIT"
  | PAYMENT -> "PAYMENT"
  | OTHER -> "OTHER"
  | REPEATPMT -> "REPEATPMT"
  | DIRECTDEP -> "DIRECTDEP"
  | CASH -> "CASH"
  | _ -> assert false

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
let get_trntype kvp = get_string "TRNTYPE" kvp |> trntype_of_string
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
    string_of_trntype trn.trntype;
    ": ";
    Currency.to_string trn.trnamt;
    " :: ";
    trn.name;
    ")"
  ]
