(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type t =
  | DIRECTDEBIT
  | PAYMENT
  | OTHER
  | REPEATPMT
  | DIRECTDEP
  | CASH
  | MATCHFAILURE

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
