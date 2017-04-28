
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

let of_contents components = 
  let dt_start = find_start components in
  let dt_end = "none" in
  let transactions = [] in
    {
      dt_start = dt_start;
      dt_end = dt_end;
      transactions = transactions;
    }
