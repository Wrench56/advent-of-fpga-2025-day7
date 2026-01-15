open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Engine.Top

let boolrows_of_string string_repr =
  List.map string_repr ~f:(fun row ->
    row
    |> String.to_list
    |> List.map ~f:(function
      | 'S' | '^' -> true
      | _ -> false))
;;

let testbench () =
  let max_iter = 10000 in
  let data_width = 16 in
  let data_depth = 16 in
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let module TestTop =
    Make_Top (struct
      let data_width = data_width
      let data_depth = data_depth
      let max_num1 = Int.pow 2 32 - 1
      let max_num2 = Int.pow 2 32 - 1
      let mem_fetch_delay = 0
      let mem_write_delay = 0
      let simd_width = 8
      let simd_cell_width = 32

      (* Standard example from Advent of Code Day 7 *)
      let rom_content =
        Some
          (boolrows_of_string
             [ ".......S........"
             ; "................"
             ; ".......^........"
             ; "................"
             ; "......^.^......."
             ; "................"
             ; ".....^.^.^......"
             ; "................"
             ; "....^.^...^....."
             ; "................"
             ; "...^.^...^.^...."
             ; "................"
             ; "..^...^.....^..."
             ; "................"
             ; ".^.^.^.^.^...^.."
             ; "................"
             ])
      ;;
    end)
  in
  let module Sim = Cyclesim.With_interface (TestTop.I) (TestTop.O) in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (TestTop.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let rec simulate curr_iter =
    if curr_iter >= max_iter
    then Stdio.printf "[stop] Max iteration of %d reached" max_iter
    else if Bits.equal !(outputs.solutions_ready) Bits.vdd
    then ()
    else (
      cycle 1;
      simulate (curr_iter + 1))
  in
  inputs.reset := Bits.vdd;
  cycle 1;
  inputs.reset := Bits.gnd;
  simulate 0;
  cycle 5;
  Stdio.printf
    "solutions_ready = %b; error = %b; solution1 = %d; solution2 = %d;\n"
    (Bits.to_bool !(outputs.solutions_ready))
    (Bits.to_bool !(outputs.error))
    (Bits.to_int !(outputs.solution1))
    (Bits.to_int !(outputs.solution2));
  waves
;;

let%expect_test "The entirety of AoC Day 7 works as expected! (Hooray!)" =
  let _ = testbench () in
  [%expect
    {|
solutions_ready = true; error = false; solution1 = 21; solution2 = 40;
|}]
;;
