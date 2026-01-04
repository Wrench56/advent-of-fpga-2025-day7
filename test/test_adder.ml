open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Adder

let testbench add_width =
  let scope = Scope.create ~flatten_design:true () in
  let module TestAdder =
    Make_Adder (struct
      let add_width = add_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestAdder.I) (TestAdder.O) in
  let sim = Sim.create (TestAdder.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let cycle n = Utils.cycle sim n in
  let add num1 num2 =
    inputs.num1 := Bits.of_int ~width:add_width num1;
    inputs.num2 := Bits.of_int ~width:add_width num2
  in
  add 1 2;
  cycle 1;
  let maxval = Int.pow 2 add_width - 1 in
  add maxval maxval;
  cycle 1;
  add 0 0;
  cycle 1;
  add maxval 1;
  cycle 1;
  waves
;;

let%expect_test "Adder works as expected" =
  let waves = testbench 16 in
  Waveform.print ~wave_width:2 ~display_width:35 ~display_height:12 waves;
  [%expect
    {|
┌Signal┐┌Waves────────────────────┐
│      ││──────┬─────┬─────┬───── │
│num1  ││ 0001 │FFFF │0000 │FFFF  │
│      ││──────┴─────┴─────┴───── │
│      ││──────┬─────┬─────┬───── │
│num2  ││ 0002 │FFFF │0000 │0001  │
│      ││──────┴─────┴─────┴───── │
│carry ││      ┌─────┐     ┌───── │
│      ││──────┘     └─────┘      │
│      ││──────┬─────┬─────────── │
│sum   ││ 0003 │FFFE │0000        │
└──────┘└─────────────────────────┘
|}]
;;
