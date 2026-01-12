open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Engine.Logic_one

let testbench () =
  let data_width = 16 in
  let data_depth = 17 in
  let mem_fetch_delay = 2 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestLogicOne =
    Make_LogicOne (struct
      let data_width = data_width
      let data_depth = data_depth
      let max_value = Int.pow 2 32
      let mem_fetch_delay = mem_fetch_delay
    end)
  in
  let module Sim = Cyclesim.With_interface (TestLogicOne.I) (TestLogicOne.O) in
  let sim = Sim.create (TestLogicOne.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  (* Standard example from Advent of Code Day 7 Part 1 *)
  let example_file =
    [| "################"
     ; ".......S........"
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
      |> Bits.concat_msb)
  in
  let memory = bitrows_of_string example_file in
  let addresses = Queue.create () in
  (* Simulate memory fetch delay *)
  for _ = 1 to mem_fetch_delay do
    Queue.enqueue addresses 0
  done;
  let cycle n =
    for _ = 1 to n do
      let addr = Bits.to_int !(outputs.mem_addr) in
      Queue.enqueue addresses addr;
      let delayed_addr = Queue.dequeue_exn addresses in
      inputs.mem_data := memory.(delayed_addr);
      Utils.cycle sim 1
    done
  in
  cycle 150;
  Stdio.printf
    "solution_ready = %b; mem_addr = %d; solution = %d\n"
    (Bits.to_bool !(outputs.solution_ready))
    (Bits.to_int !(outputs.mem_addr))
    (Bits.to_int !(outputs.solution));
  waves
;;

let%expect_test "Part 1 Solver Engine works as expected!" =
  let _ = testbench () in
  [%expect
    {|
    solution_ready = true; mem_addr = 16; solution = 21
|}]
;;
