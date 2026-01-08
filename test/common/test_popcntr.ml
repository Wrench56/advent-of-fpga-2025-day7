open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Popcntr

let testbench data_width =
  let scope = Scope.create ~flatten_design:true () in
  let module TestPopcounter =
    Make_Popcounter (struct
      let data_width = data_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestPopcounter.I) (TestPopcounter.O) in
  let sim = Sim.create (TestPopcounter.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let set_bits bits = inputs.data := Bits.of_string bits in
  let enable () = inputs.enable := Bits.vdd in
  let disable () = inputs.enable := Bits.gnd in
  disable ();
  set_bits "8'b00000000";
  cycle 1;
  set_bits "8'b01010101";
  enable ();
  cycle 1;
  set_bits "8'b11111111";
  cycle 1;
  disable ();
  cycle 1;
  waves
;;

let%expect_test "Combinational Popcounter works as expected" =
  let waves = testbench 8 in
  Waveform.print ~wave_width:2 ~display_width:35 ~display_height:12 waves;
  [%expect
    {|
┌Signal┐┌Waves────────────────────┐
│enable││      ┌───────────┐      │
│      ││──────┘           └───── │
│      ││──────┬─────┬─────────── │
│data  ││ 00   │55   │FF          │
│      ││──────┴─────┴─────────── │
│      ││──────┬─────┬─────┬───── │
│count ││ 0    │4    │8    │0     │
│      ││──────┴─────┴─────┴───── │
│ready ││      ┌───────────┐      │
│      ││──────┘           └───── │
└──────┘└─────────────────────────┘
|}]
;;
