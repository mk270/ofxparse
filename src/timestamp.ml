(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type t = {
    date : string;
    time : string;
    tz : string;
}

let of_string s = 
  let tz =
    let offset = 1 + String.index s '[' in
    let closing = String.index s ']' in
      String.sub s (offset) (closing - offset)
  in
    {
      date = String.sub s 0 8;
      time = String.sub s 8 6;
      tz   = tz;
    }

let to_string ts = 
  (String.sub ts.date 0 4) ^ "-" ^
  (String.sub ts.date 4 2) ^ "-" ^
  (String.sub ts.date 6 2)
