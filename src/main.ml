(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

open Ast

exception Wrong_tag of (string * string)

let main nodes debug dump_time_range dump_trans =
  let debug_log s =
    if debug
    then Printf.fprintf stderr "%s" s
    else ()
  in

  let assert_tag_name node expected_name =
    let observed_name = string_of_node_tag_name node in
      if observed_name = expected_name
      then ()
      else raise (Wrong_tag (observed_name, expected_name))
  in

  let transaction_of_node_contents = function
    | [Kvp a; Kvp b; Kvp c; Kvp d; Kvp e] ->
       Stmttrn.of_tuples a b c d e
    | _ -> assert false
  in

  let visit_banktranlist contents_ =
    let rec visit_banktran = function
      | Node elt -> string_of_node_tag_name elt |>
          handle_banktran elt.node_contents
      | Kvp x -> Banktranlist.parse_tuple x
    and handle_banktran contents = function
      | "STMTTRN" -> Banktranlist.make_transaction
         (transaction_of_node_contents contents)
      | s -> debug_log s; assert false
    and visit_banktranlist = function
      | [] -> []
      | hd :: tl -> visit_banktran hd :: visit_banktranlist tl
    in
      visit_banktranlist contents_ |> Banktranlist.of_contents
  in

  let rec visit_stmtr = function
    | Node elt ->
       (match (string_of_node_tag_name elt) with
         | "BANKTRANLIST" -> Some (visit_banktranlist elt.node_contents)
         | "LEDGERBAL"
         | "BANKACCTFROM" -> None
         | s -> debug_log s; assert false)
    | Kvp x -> Ast.Dump.tuple x |> debug_log ; None
  and visit_stmtrs : node_contents list -> Banktranlist.t option list =
    function
    | [] -> []
    | hd :: tl -> visit_stmtr hd :: visit_stmtrs tl
  in

  let rec visit_stmttrnr = function
    | Node elt ->
       (match (string_of_tag elt.tag_name) with
         | "STMTRS" -> visit_stmtrs elt.node_contents
         | "STATUS" -> []
         | s -> debug_log s; assert false
       )
    | Kvp x -> Ast.Dump.tuple x |> debug_log ; []
  and visit_stmttrnrs = function
    | [] -> []
    | hd :: tl -> visit_stmttrnr hd :: visit_stmttrnrs tl
  in

  let visit_bankmsgsrv1 = function
    | [Node elt] ->
       assert_tag_name elt "STMTTRNRS";
      visit_stmttrnrs elt.node_contents
    | _ -> assert false
  in

  let visit_ofx = function
    | [Node discard; Node bankmsgsrv1;] ->
       assert_tag_name bankmsgsrv1 "BANKMSGSRSV1";
      visit_bankmsgsrv1 bankmsgsrv1.node_contents
    | _ -> assert false
  in

  let visit_top = function
    | Root_node n ->
       assert (string_of_tag n.tag_name = "OFX");
       visit_ofx n.node_contents
    | _ -> assert false;
  in

  let rec dump_node_contents = function
    | [] -> ()
    | None :: tl -> dump_node_contents tl
    | Some hd :: tl ->
       (match (dump_time_range, dump_trans) with
        | true, true -> assert false
        | true, false ->
           Banktranlist.dump_time_range hd |> print_endline
        | false, true ->
           Banktranlist.dump_transactions hd |> List.iter print_endline
        | false, false -> ());
       dump_node_contents tl
  in
    (* Dump.registry headers |> debug_log;
       Dump.registry nodes |> debug_log; *)
    visit_top nodes |> List.iter dump_node_contents

let nodes_of_channel input_channel debug =
  try
    let lexbuf = Lexing.from_channel input_channel in
    let headers = Parse_ofx.parse Lexer.header_token lexbuf debug in
    let nodes = Parse_ofx.parse Lexer.token lexbuf debug in
      ignore headers;
      nodes
    with Wrong_tag (o, e) as exc ->
      (
        ("Observed: " ^ o ^ "; expected: " ^ e) |> print_endline;
        raise exc
      )

let nodes_of_filename filename debug =
  let ic = open_in_bin filename in
    try let nodes = nodes_of_channel ic debug in
          main nodes debug false false;
          close_in ic;
          nodes
  with exc ->
       close_in_noerr ic;
       raise exc

let _ =
  let filenames = ref (Array.make 0 "") in
  let debug = ref false in
  let dump_time_range = ref false in
  let dump_trans = ref false in
  let arg_specs = [
      ("--debug", Arg.Set debug, "Debug mode");
      ("--dump-time-range", Arg.Set dump_time_range, "Dump time range of file");
      ("--dump-transactions", Arg.Set dump_trans, "Dump transactions");
    ]
  in
  let usage = "No usage message specified yet" in
  let anon = fun s ->
    filenames := Array.append !filenames [| s |]
  in
    Arg.parse arg_specs anon usage;

    match (!filenames, !dump_time_range, !dump_trans) with
    | [| |], _, _ ->
       let nodes = nodes_of_channel stdin !debug in
         main nodes !debug !dump_time_range !dump_trans

    | [| f |], false, false ->
       ignore (nodes_of_filename f !debug)

    | ff, false, false ->
       Array.iter (fun f ->
           ignore (nodes_of_filename f !debug)
         ) ff

    | _, true, _
    | _, _, true
      -> assert false
