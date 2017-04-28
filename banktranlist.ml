
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

let find_start cc =
  let matcher = function
    | Start _ -> true
    | _ -> false
  in
  let matched = List.filter matcher cc in
    match matched with
    | [Start s] -> s
    | _ -> assert false

let of_contents components = 
  let dt_start = "none" in
  let dt_end = "none" in
  let transactions = [] in
    {
      dt_start = dt_start;
      dt_end = dt_end;
      transactions = transactions;
    }
