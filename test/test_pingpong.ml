open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Pingpong
open Utils

let testbench () =
  let data_width = 32 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestPingPongBuffer =
    Make_PingPongBuffer (struct
      let data_width = data_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestPingPongBuffer.I) (TestPingPongBuffer.O)
  in
  let sim = Sim.create (TestPingPongBuffer.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let cycle n =
    for _ = 1 to n do
      Cyclesim.cycle sim
    done
  in
  let set_buffer value = inputs.input := bits_of_string_be ~width:data_width value in
  let set_zero () = inputs.input := Bits.of_int ~width:data_width 0 in
  let enable () = inputs.enable := Bits.vdd in
  let disable () = inputs.enable := Bits.gnd in
  let clear () =
    inputs.clear := Bits.vdd;
    cycle 1;
    inputs.clear := Bits.gnd
  in
  let swp_buffer () =
    inputs.swap := Bits.vdd;
    cycle 1;
    inputs.swap := Bits.gnd
  in
  disable ();
  clear ();
  enable ();
  set_buffer "DEAD";
  swp_buffer ();
  cycle 2;
  set_buffer "BEEF";
  cycle 2;
  swp_buffer ();
  cycle 2;
  set_zero ();
  clear ();
  cycle 1;
  swp_buffer ();
  enable ();
  cycle 1;
  waves
;;

let%expect_test "todo" =
  let waves = testbench () in
  Waveform.print ~wave_width:2 ~display_width:100 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
│clear             ││──────┐                                               ┌─────┐                 │
│                  ││      └───────────────────────────────────────────────┘     └─────────────────│
│enable            ││      ┌───────────────────────────────────────────────────────────────────────│
│                  ││──────┘                                                                       │
│                  ││──────┬─────────────────┬─────────────────────────────┬───────────────────────│
│input             ││ 0000.│44454144         │42454546                     │00000000               │
│                  ││──────┴─────────────────┴─────────────────────────────┴───────────────────────│
│swap              ││      ┌─────┐                       ┌─────┐                       ┌─────┐     │
│                  ││──────┘     └───────────────────────┘     └───────────────────────┘     └─────│
│                  ││────────────┬─────────────────────────────┬─────────────────┬─────────────────│
│output            ││ 00000000   │44454144                     │42454546         │00000000         │
│                  ││────────────┴─────────────────────────────┴─────────────────┴─────────────────│
│                  ││                                                                              │
│                  ││                                                                              │
│                  ││                                                                              │
│                  ││                                                                              │
└──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
|}]
;;
