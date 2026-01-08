open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Counter

let testbench ~saturating =
  let max_num = 7 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestCounter =
    Make_Counter (struct
      let max_num = max_num
      let saturating = saturating
    end)
  in
  let module Sim = Cyclesim.With_interface (TestCounter.I) (TestCounter.O) in
  let sim = Sim.create (TestCounter.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let clear () =
    inputs.clear := Bits.of_int 1 ~width:1;
    Cyclesim.cycle sim;
    inputs.clear := Bits.of_int 0 ~width:1
  in
  let increment () =
    inputs.increment := Bits.of_int 1 ~width:1;
    Cyclesim.cycle sim;
    inputs.increment := Bits.of_int 0 ~width:1
  in
  clear ();
  for _ = 0 to 10 do
    increment ()
  done;
  (* Wait *)
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  clear ();
  Cyclesim.cycle sim;
  waves
;;

let%expect_test "Zero-wrapping Counter works as expected" =
  let waves = testbench ~saturating: false in
  Waveform.print ~wave_width:2 ~display_width:120 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
│clear             ││──────┐                                                                             ┌─────┐       │
│                  ││      └─────────────────────────────────────────────────────────────────────────────┘     └─────  │
│increment         ││      ┌─────────────────────────────────────────────────────────────────┐                         │
│                  ││──────┘                                                                 └───────────────────────  │
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────────┬─────  │
│count             ││ 0          │1    │2    │3    │4    │5    │6    │7    │0    │1    │2    │3                │0      │
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────────┴─────  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
└──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
  |}]
;;

let%expect_test "Saturating Counter works as expected" =
    let waves = testbench ~saturating:true in
    Waveform.print ~wave_width:2 ~display_width:120 waves;
    [%expect
  {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
│clear             ││──────┐                                                                             ┌─────┐       │
│                  ││      └─────────────────────────────────────────────────────────────────────────────┘     └─────  │
│increment         ││      ┌─────────────────────────────────────────────────────────────────┐                         │
│                  ││──────┘                                                                 └───────────────────────  │
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────────────────────────────────┬─────  │
│count             ││ 0          │1    │2    │3    │4    │5    │6    │7                                        │0      │
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────────────────────────────────┴─────  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
│                  ││                                                                                                  │
└──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
  |}]
