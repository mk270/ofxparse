(*
  ofxparse, a parser for OFX files, by Martin Keegan

  Copyright (C) 2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache Licence v2.0.
*)

type t = {
  whole : int;
  dec : int;
}

let of_string s =
  try
    let point = String.index s '.' in
    let w = String.sub s 0 point |> int_of_string in
    let dec = String.sub s (point + 1) 2 |> int_of_string in
      {
        whole = w;
        dec = dec;
      }
  with Not_found ->
    let w = int_of_string s in
      { 
        whole = w;
        dec = 0;
      }
