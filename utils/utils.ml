open Base
open Hardcaml

(* TODO: There has to be a built-in function for this... *)
let bits_of_string_be ~width (s : string) : Bits.t =
  Bits.of_int
    ~width
    ((fun s -> String.fold s ~init:0 ~f:(fun acc c -> (acc lsl 8) lor Char.to_int c)) s)
;;
