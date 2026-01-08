open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Adder

let run_adder ~num_inputs ~add_width ~ops =
  let scope = Scope.create ~flatten_design:true () in
  let module TestAdderN =
    Make_AdderN (struct
      let num_inputs = num_inputs
      let add_width = add_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestAdderN.I) (TestAdderN.O) in
  let sim = Sim.create (TestAdderN.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let cycle n = Utils.cycle sim n in
  let drive op =
    assert (List.length op = num_inputs + 1);
    for i = 0 to num_inputs - 2 do
      inputs.nums.(i) := Bits.of_int ~width:add_width (List.nth_exn op i)
    done;
    inputs.enable := Bits.of_int ~width:1 (List.last_exn op)
  in
  List.iter ops ~f:(fun op ->
    drive op;
    cycle 1);
  waves
;;

let maxval width = Int.pow 2 width - 1

let%expect_test "Adder2 works as expected" =
  let width = 16 in
  let mval = maxval width in
  let en = 1 in
  let di = 0 in
  let waves =
    run_adder
      ~num_inputs:2
      ~add_width:16
      ~ops:
        [ [ 1; 1; di ]; [ 1; 2; en ]; [ mval; mval; en ]; [ 0; 0; en ]; [ mval; 1; en ] ]
  in
  Waveform.print ~wave_width:2 ~display_width:42 ~display_height:17 waves;
  [%expect
    {|
┌Signals─┐┌Waves─────────────────────────┐
│enable  ││      ┌───────────────────────│
│        ││──────┘                       │
│        ││────────────┬─────┬─────┬─────│
│nums0   ││ 0001       │FFFF │0000 │FFFF │
│        ││────────────┴─────┴─────┴─────│
│        ││──────────────────────────────│
│nums1   ││ 0000                         │
│        ││──────────────────────────────│
│carry   ││                              │
│        ││──────────────────────────────│
│ready   ││      ┌───────────────────────│
│        ││──────┘                       │
│        ││──────┬─────┬─────┬─────┬─────│
│sum     ││ 0000 │0001 │FFFF │0000 │FFFF │
│        ││──────┴─────┴─────┴─────┴─────│
└────────┘└──────────────────────────────┘
|}]
;;

let%expect_test "Adder4 works as expected" =
  let width = 16 in
  let mval = maxval width in
  let en = 1 in
  let di = 0 in
  let waves =
    run_adder
      ~num_inputs:4
      ~add_width:16
      ~ops:
        [ [ 1; 1; 1; 1; di ]
        ; [ 1; 2; 3; 4; en ]
        ; [ mval; mval; mval; mval; en ]
        ; [ 0; 0; 0; 0; en ]
        ; [ mval; 1; 0; 0; en ]
        ]
  in
  Waveform.print ~wave_width:2 ~display_width:42 ~display_height:24 waves;
  [%expect
    {|
┌Signals─┐┌Waves─────────────────────────┐
│enable  ││      ┌───────────────────────│
│        ││──────┘                       │
│        ││────────────┬─────┬─────┬─────│
│nums0   ││ 0001       │FFFF │0000 │FFFF │
│        ││────────────┴─────┴─────┴─────│
│        ││──────┬─────┬─────┬─────┬─────│
│nums1   ││ 0001 │0002 │FFFF │0000 │0001 │
│        ││──────┴─────┴─────┴─────┴─────│
│        ││──────┬─────┬─────┬───────────│
│nums2   ││ 0001 │0003 │FFFF │0000       │
│        ││──────┴─────┴─────┴───────────│
│        ││──────────────────────────────│
│nums3   ││ 0000                         │
│        ││──────────────────────────────│
│        ││────────────┬─────┬─────┬─────│
│carry   ││ 0          │2    │0    │1    │
│        ││────────────┴─────┴─────┴─────│
│ready   ││      ┌───────────────────────│
│        ││──────┘                       │
│        ││──────┬─────┬─────┬───────────│
│sum     ││ 0000 │0006 │FFFD │0000       │
│        ││──────┴─────┴─────┴───────────│
└────────┘└──────────────────────────────┘
|}]
;;
