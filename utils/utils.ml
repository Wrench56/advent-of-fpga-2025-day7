open Base
open Hardcaml

let bits_of_string_be ~width (s : string) : Bits.t =
  let bits =
    s
    |> String.to_list
    |> List.map ~f:(fun c -> Bits.of_int ~width:8 (Char.to_int c))
    |> Bits.concat_msb
  in
  Bits.uresize bits width
;;

let cycle sim n =
  assert (n > 0);
  for _ = 1 to n do
    Cyclesim.cycle sim
  done
;;
