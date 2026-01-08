open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Accumulator

let testbench ~saturating =
  let max_value = 255 + 1 in
  let add_width = 8 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestAccumulator =
    Make_Accumulator (struct
      let max_value = max_value
      let add_width = add_width
      let saturating = saturating
    end)
  in
  let module Sim = Cyclesim.With_interface (TestAccumulator.I) (TestAccumulator.O) in
  let sim = Sim.create (TestAccumulator.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let clear () =
    inputs.clear := Bits.vdd;
    cycle 1;
    inputs.clear := Bits.gnd;
    inputs.enable := Bits.gnd
  in
  let add num =
    inputs.add := Bits.of_int num ~width:add_width;
    cycle 1;
    inputs.add := Bits.of_int num ~width:add_width
  in
  clear ();
  add 102;
  inputs.enable := Bits.vdd;
  add 102;
  add 255;
  add 154;
  add 10;
  add 0;
  add 10;
  add 0;
  cycle 10;
  waves
;;

let%expect_test "Zero-wrapping Accumulator works as expected" =
  let waves = testbench ~saturating:false in
  Waveform.print ~wave_width:2 ~display_width:80 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
│clear             ││──────┐                                                   │
│                  ││      └───────────────────────────────────────────────────│
│enable            ││            ┌─────────────────────────────────────────────│
│                  ││────────────┘                                             │
│                  ││──────┬───────────┬─────┬─────┬─────┬─────┬─────┬─────────│
│add               ││ 00   │66         │FF   │9A   │0A   │00   │0A   │00       │
│                  ││──────┴───────────┴─────┴─────┴─────┴─────┴─────┴─────────│
│ready             ││──────────────────────────────────────────────────────────│
│                  ││                                                          │
│                  ││──────────────────┬─────┬─────┬─────┬───────────┬─────────│
│sum               ││ 000              │066  │165  │1FF  │009        │013      │
│                  ││──────────────────┴─────┴─────┴─────┴───────────┴─────────│
│                  ││                                                          │
│                  ││                                                          │
│                  ││                                                          │
│                  ││                                                          │
└──────────────────┘└──────────────────────────────────────────────────────────┘
|}]
;;

let%expect_test "Saturating Accumulator works as expected" =
  let waves = testbench ~saturating:true in
  Waveform.print ~wave_width:2 ~display_width:80 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
│clear             ││──────┐                                                   │
│                  ││      └───────────────────────────────────────────────────│
│enable            ││            ┌─────────────────────────────────────────────│
│                  ││────────────┘                                             │
│                  ││──────┬───────────┬─────┬─────┬─────┬─────┬─────┬─────────│
│add               ││ 00   │66         │FF   │9A   │0A   │00   │0A   │00       │
│                  ││──────┴───────────┴─────┴─────┴─────┴─────┴─────┴─────────│
│ready             ││──────────────────────────────────────────────────────────│
│                  ││                                                          │
│                  ││──────────────────┬─────┬─────────────────────────────────│
│sum               ││ 000              │066  │100                              │
│                  ││──────────────────┴─────┴─────────────────────────────────│
│                  ││                                                          │
│                  ││                                                          │
│                  ││                                                          │
│                  ││                                                          │
└──────────────────┘└──────────────────────────────────────────────────────────┘
|}]
;;
