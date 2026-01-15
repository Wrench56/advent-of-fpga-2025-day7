open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Engine.Manifold_engine

let testbench () =
  let data_width = 16 in
  let data_depth = 16 in
  let mem_delay = 2 in
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let module TestManifoldEngine =
    Make_ManifoldEngine (struct
      let data_width = data_width
      let data_depth = data_depth
      let simd_cell_width = 16
      let simd_width = 8
      let max_value = 1024
      let mem_fetch_delay = mem_delay
      let mem_write_delay = mem_delay
    end)
  in
  let module Sim = Cyclesim.With_interface (TestManifoldEngine.I) (TestManifoldEngine.O)
  in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (TestManifoldEngine.create scope)
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  (* Standard example from Advent of Code Day 7 *)
  let example_file =
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
    ]
  in
  let boolrows_of_string string_repr =
    List.map string_repr ~f:(fun row ->
      row
      |> String.to_list
      |> List.map ~f:(function
        | 'S' | '^' -> true
        | _ -> false))
  in
  let shl_lst lst = false :: List.drop_last_exn lst in
  let shr_lst lst = List.tl_exn lst @ [ false ] in
  let hit_splitters init_beam splitters =
    let rec simulate beamrow hit_splitters = function
      | [] -> List.rev hit_splitters
      | row :: tail ->
        let hit = List.map2_exn beamrow row ~f:( && ) in
        let nbeamrow =
          List.map3_exn
            (List.map2_exn beamrow (List.map hit ~f:not) ~f:( && ))
            (shl_lst hit)
            (shr_lst hit)
            ~f:(fun a b c -> a || b || c)
        in
        simulate nbeamrow (hit :: hit_splitters) tail
    in
    simulate init_beam [] splitters
  in
  let splitter_lst = boolrows_of_string example_file in
  let simul_tbl =
    let hit_splitters =
      hit_splitters
        (List.hd_exn splitter_lst)
        (List.init data_width ~f:(fun _ -> false) :: List.tl_exn splitter_lst)
    in
    List.hd_exn splitter_lst :: List.tl_exn hit_splitters
  in
  let set_splitters (input : Bits.t ref TestManifoldEngine.I.t) splitters mem_slot =
    let b2blst lst = Bits.concat_lsb (List.map lst ~f:Bits.of_bool) in
    let splitters = List.nth_exn splitters mem_slot in
    input.hit_splitters.shl := b2blst (shl_lst splitters);
    input.hit_splitters.cen := b2blst splitters;
    input.hit_splitters.shr := b2blst (shr_lst splitters)
  in
  let cycle n = Utils.cycle sim n in
  inputs.reset := Bits.vdd;
  cycle 1;
  inputs.reset := Bits.gnd;
  inputs.enable := Bits.vdd;
  cycle 1;
  let sim
        (i : Bits.t ref TestManifoldEngine.I.t)
        (o : Bits.t ref TestManifoldEngine.O.t)
        sim_limit
    =
    let rec intr_sim n curr_hit_idx =
      if n > sim_limit || Bits.to_bool !(o.solution_ready) || curr_hit_idx > data_depth
      then (
        let () =
          Stdio.printf
            "Simulation stopped! Reason: %s\n"
            (if n > sim_limit
             then "simulation cycle limit reached."
             else if Bits.to_bool !(o.solution_ready)
             then "solution found!"
             else "memory depth reached.")
        in
        cycle 5)
      else if n = 0
      then (
        Stdio.printf "[mem] Boot\n";
        set_splitters i simul_tbl curr_hit_idx;
        i.sim_ready := Bits.vdd;
        intr_sim (n + 1) (curr_hit_idx + 1))
      else if Bits.to_bool !(o.next_iter_ready)
      then (
        assert (List.length splitter_lst > curr_hit_idx);
        Stdio.printf "[mem] Next\n";
        i.sim_ready := Bits.gnd;
        cycle mem_delay;
        set_splitters i simul_tbl curr_hit_idx;
        i.sim_ready := Bits.vdd;
        intr_sim (n + 1) (curr_hit_idx + 1))
      else (
        cycle 1;
        intr_sim (n + 1) curr_hit_idx)
    in
    intr_sim 0 0
  in
  sim inputs outputs 2000;
  Stdio.printf
    "solution_ready = %b; overflow = %b; solution = %d\n"
    (Bits.to_bool !(outputs.solution_ready))
    (Bits.to_bool !(outputs.overflow))
    (Bits.to_int !(outputs.solution));
  waves
;;

let%expect_test "Manifold Engine works as expected!" =
  let _waves = testbench () in
  [%expect
    {|
[mem] Boot
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
[mem] Next
Simulation stopped! Reason: solution found!
solution_ready = true; overflow = false; solution = 40
|}]
;;
