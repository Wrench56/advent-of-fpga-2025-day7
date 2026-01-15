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

type grid =
  { width : int
  ; height : int
  ; rows : string array
  }

let load_grid (path : string) : grid =
  let lines = In_channel.with_open_text path In_channel.input_lines in
  match lines with
  | [] -> failwith "Gridfile is empty!"
  | first :: rest ->
    let width = String.length first in
    if width = 0 then failwith "Gridfile's first line is empty!";
    List.iteri (first :: rest) ~f:(fun idx line ->
      let w = String.length line in
      if w <> width
      then
        failwith
          (Printf.sprintf
             "Gridfile has inconsistent line width at line %d: got %d expected %d"
             (idx + 1)
             w
             width));
    let rows = Array.of_list (first :: rest) in
    let height = Array.length rows in
    { width; height; rows }
;;
