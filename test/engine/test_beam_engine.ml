open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Engine.Beam_engine

let testbench () =
  let max_iter = 1000 in
  let data_width = 16 in
  let data_depth = 16 in
  let mem_fetch_delay = 2 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestBeamEngine =
    Make_BeamEngine (struct
      let data_width = data_width
      let data_depth = data_depth
      let max_value = Int.pow 2 32
    end)
  in
  let module Sim = Cyclesim.With_interface (TestBeamEngine.I) (TestBeamEngine.O) in
  let sim = Sim.create (TestBeamEngine.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  (* Standard example from Advent of Code Day 7 Part 1 *)
  let example_file =
    [| ".......S........"
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
    |]
  in
  let bitrows_of_string string_repr =
    Array.map string_repr ~f:(fun row ->
      row
      |> String.to_list
      |> List.map ~f:(function
        | 'S' | '^' -> Bits.vdd
        | _ -> Bits.gnd)
      |> Bits.concat_lsb)
  in
  let memory = bitrows_of_string example_file in
  let cycle n = Utils.cycle sim n in
  let rec simulate curr_iter curr_slot =
    if curr_iter >= max_iter
    then ()
    else if Bits.equal !(outputs.next_iter_ready) Bits.vdd
    then (
      cycle mem_fetch_delay;
      inputs.mem_data := memory.(curr_slot);
      inputs.mem_ready := Bits.vdd;
      cycle 1;
      inputs.mem_ready := Bits.gnd;
      simulate (curr_iter + 1) (curr_slot + 1);
      ())
    else if Bits.equal !(outputs.solution_ready) Bits.vdd
    then ()
    else (
      cycle 1;
      simulate (curr_iter + 1) curr_slot)
  in
  simulate 0 0;
  Stdio.printf
    "solution_ready = %b; solution = %d\n"
    (Bits.to_bool !(outputs.solution_ready))
    (Bits.to_int !(outputs.solution));
  waves
;;

let%expect_test "Beam Engine works as expected!" =
  let _ = testbench () in
  [%expect
    {|
solution_ready = true; solution = 21
|}]
;;
