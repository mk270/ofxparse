(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type t = {
  dt_start : string;
  dt_end : string;
  transactions : Stmttrn.t list
}

type component = 
  | Start of string
  | End of string
  | Transaction of Stmttrn.t

let parse_tuple x =
  let id, v = x in
  let ide = Ast.string_of_ident id in
    match ide with
    | "DTSTART" -> Start v
    | "DTEND" -> End v
    | _ -> assert false

let find_component cc matcher extractor =
  List.filter matcher cc |> extractor

let find_start cc =
  let matcher = function
    | Start _ -> true
    | _ -> false
  in
  let extractor = function
    | [Start s] -> s
    | [] -> raise Not_found
    | _ -> assert false
  in
    find_component cc matcher extractor

let find_end cc =
  let matcher = function
    | End _ -> true
    | _ -> false
  in
  let extractor = function
    | [End s] -> s
    | [] -> raise Not_found
    | _ -> assert false
  in
    find_component cc matcher extractor

let find_transactions =
  List.filter (function | Transaction t -> true | _ -> false)

let stmttrn_of_transaction = function
  | Transaction t -> t
  | _ -> assert false

let of_contents components = 
  let dt_start = find_start components in
  let dt_end = find_end components in
  let transactions = find_transactions components |>
      List.map stmttrn_of_transaction
  in
    {
      dt_start = dt_start;
      dt_end = dt_end;
      transactions = transactions;
    }
