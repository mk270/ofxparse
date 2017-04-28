open Ast

exception Wrong_tag of (string * string)

let main debug =
	let _ = Parsing.set_trace debug in

	let lexbuf = Lexing.from_channel stdin in

	let report_lex () =
		let pos = lexbuf.Lexing.lex_curr_p in
		let lnum = pos.Lexing.pos_lnum in
		let cnum = pos.Lexing.pos_cnum in
		let lexeme = Lexing.lexeme lexbuf in 
		Printf.printf "line: %d, byte: %d, `%s'\n" 
			lnum cnum lexeme
	in

	let parse lx = 
		try Parser.input lx lexbuf
		with 
		| Failure f ->
			report_lex ();
			print_endline f;
			failwith f
		| Parsing.Parse_error ->
			report_lex ();
			failwith "parse error"
		| Lexer.Unexpected_token ->
			report_lex ();
			failwith "lexer error"
	in

    let assert_tag_name node expected_name =
      let observed_name = string_of_tag node.tag_name in
        if observed_name = expected_name
        then ()
        else raise (Wrong_tag (observed_name, expected_name))
    in

    let transaction_of_node_contents = function
      | [Kvp a; Kvp b; Kvp c; Kvp d; Kvp e] ->
         Stmttrn.of_tuples a b c d e
      | _ -> assert false
    in

    let rec visit_banktran = function
      | Node elt ->
         (assert (string_of_tag elt.tag_name = "STMTTRN");
          print_endline "TRAN";
          Banktranlist.Transaction
            (transaction_of_node_contents elt.node_contents))
      | Kvp x -> Banktranlist.parse_tuple x
    and visit_banktranlist = function
      | [] -> []
      | hd :: tl -> visit_banktran hd :: visit_banktranlist tl
    in

    let rec visit_stmtr = function
      | Node elt ->
         (match (string_of_tag elt.tag_name) with
           | "BANKTRANLIST" -> visit_banktranlist elt.node_contents
           | "LEDGERBAL"
           | "BANKACCTFROM" -> []
           | s -> print_endline s; assert false)
      | Kvp x -> Dump.tuple x |> print_endline ; []
    and visit_stmtrs = function
      | [] -> []
      | hd :: tl -> visit_stmtr hd :: visit_stmtrs tl
    in

    let rec visit_stmttrnr = function 
      | Node elt ->
         (match (string_of_tag elt.tag_name) with
           | "STMTRS" -> visit_stmtrs elt.node_contents
           | "STATUS" -> []
           | s -> print_endline s; assert false
         )
      | Kvp x -> Dump.tuple x |> print_endline ; []
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

    let dump_node_contents = function
      | _ -> print_endline "OOK"
    in

      try
        let headers = parse Lexer.header_token in
        let nodes = parse Lexer.token in
          Dump.registry headers |> print_endline;
          Dump.registry nodes |> print_endline;
          visit_top nodes |> List.iter dump_node_contents
      with Wrong_tag (o, e) ->
        ("Observed: " ^ o ^ "; expected: " ^ e) |> print_endline

let _ = 
	match Sys.argv with
	| [| progname; "--debug" |] -> main true
	| _                         -> main false
