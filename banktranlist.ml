
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
