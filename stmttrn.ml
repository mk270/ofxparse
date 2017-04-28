
type t = {
  trntype : string;
  dtposted : string;
  trnamt : Currency.t;
  fitid : string;
  name : string;
}

let of_tuples a b c d e = {
  trntype = "string";
  dtposted = "string";
  trnamt = Currency.of_string "-1324.10";
  fitid = "string";
  name = "string";
}
