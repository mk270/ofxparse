
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

    let dump_parsed lx =
      let parsed = parse lx in
        Eval.dump_registry parsed |> print_endline
    in

      [
        Lexer.header_token;
        Lexer.token;
      ]
      |> List.map dump_parsed

let _ = 
	match Sys.argv with
	| [| progname; "--debug" |] -> main true
	| _                         -> main false
