open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Stencil.Stencil

let testbench () =
  let data_width = 16 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestStencil =
    Make_Stencil (struct
      let data_width = data_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestStencil.I) (TestStencil.O) in
  let sim = Sim.create (TestStencil.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let enable () = inputs.enable := Bits.vdd in
  let disable () = inputs.enable := Bits.gnd in
  let clear () =
    inputs.clear := Bits.vdd;
    cycle 1;
    inputs.clear := Bits.gnd
  in
  let drive_stencil hitl ccurrl =
    inputs.hit_range := List.rev hitl |> Bits.of_bit_list;
    inputs.nw := List.nth_exn ccurrl 0 |> Bits.of_int ~width:data_width;
    inputs.no := List.nth_exn ccurrl 1 |> Bits.of_int ~width:data_width;
    inputs.ne := List.nth_exn ccurrl 2 |> Bits.of_int ~width:data_width;
    cycle 1
  in
  let maxval = Int.pow 2 data_width - 1 in
  let ottl = [ 1; 2; 3 ] in
  let maxl = [ maxval; maxval; maxval ] in
  clear ();
  disable ();
  cycle 1;
  drive_stencil [ 1; 1; 1 ] ottl;
  enable ();
  drive_stencil [ 1; 0; 1 ] ottl;
  drive_stencil [ 0; 1; 1 ] ottl;
  drive_stencil [ 1; 0; 0 ] ottl;
  drive_stencil [ 1; 1; 1 ] ottl;
  drive_stencil [ 1; 0; 1 ] maxl;
  disable ();
  cycle 1;
  enable ();
  drive_stencil [ 0; 1; 0 ] maxl;
  waves
;;

let%expect_test "Combinational Stencil works as expected" =
  let waves = testbench () in
  Waveform.print ~wave_width:2 ~display_width:82 ~display_height:23 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
│enable            ││                  ┌─────────────────────────────┐     ┌─────│
│                  ││──────────────────┘                             └─────┘     │
│                  ││────────────┬─────┬─────┬─────┬─────┬─────┬───────────┬─────│
│hit_range         ││ 0          │7    │5    │6    │1    │7    │5          │2    │
│                  ││────────────┴─────┴─────┴─────┴─────┴─────┴───────────┴─────│
│                  ││────────────┬─────────────────────────────┬─────────────────│
│ne                ││ 0000       │0003                         │FFFF             │
│                  ││────────────┴─────────────────────────────┴─────────────────│
│                  ││────────────┬─────────────────────────────┬─────────────────│
│no                ││ 0000       │0002                         │FFFF             │
│                  ││────────────┴─────────────────────────────┴─────────────────│
│                  ││────────────┬─────────────────────────────┬─────────────────│
│nw                ││ 0000       │0001                         │FFFF             │
│                  ││────────────┴─────────────────────────────┴─────────────────│
│                  ││──────────────────┬─────┬───────────┬─────┬─────┬───────────│
│cell              ││ 0000             │0006 │0003       │0004 │FFFD │0000       │
│                  ││──────────────────┴─────┴───────────┴─────┴─────┴───────────│
│overflow          ││                                          ┌─────┐           │
│                  ││──────────────────────────────────────────┘     └───────────│
│ready             ││                  ┌─────────────────────────────┐     ┌─────│
│                  ││──────────────────┘                             └─────┘     │
└──────────────────┘└────────────────────────────────────────────────────────────┘
|}]
;;
