open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Bram

let testbench () =
  let data_width = 32 in
  let bram_depth = 256 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestBRAM =
    Make_BRAM (struct
      let data_width = data_width
      let depth = bram_depth
    end)
  in
  let addr_width = Int.ceil_log2 bram_depth in
  let module Sim = Cyclesim.With_interface (TestBRAM.I) (TestBRAM.O) in
  let sim = Sim.create (TestBRAM.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let read ~addr =
    inputs.read_enable := Bits.vdd;
    inputs.read_addr := addr |> Bits.of_int ~width:addr_width
  in
  let write ~addr ~data =
    inputs.write_enable := Bits.vdd;
    inputs.write_addr := addr |> Bits.of_int ~width:addr_width;
    inputs.write_data := data
  in
  write ~addr:0x1 ~data:(Utils.bits_of_string_be ~width:data_width "Hell");
  read ~addr:0x1;
  cycle 1;
  (* Write is done *)
  cycle 1;
  (* Read is done *)
  write ~addr:0x2 ~data:!(outputs.read_data);
  cycle 1;
  waves
;;

let%expect_test "Memory works correctly" =
  let waves = testbench () in
  Waveform.print ~wave_width:4 waves;
  [%expect
    {|
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│clock          ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌│
│               ││     └────┘    └────┘    └────┘    └────┘    └────┘│
│               ││──────────────────────────────                     │
│read_addr      ││ 01                                                │
│               ││──────────────────────────────                     │
│read_enable    ││──────────────────────────────                     │
│               ││                                                   │
│               ││────────────────────┬─────────                     │
│write_addr     ││ 01                 │02                            │
│               ││────────────────────┴─────────                     │
│               ││──────────────────────────────                     │
│write_data     ││ 48656C6C                                          │
│               ││──────────────────────────────                     │
│write_enable   ││──────────────────────────────                     │
│               ││                                                   │
│               ││────────────────────┬─────────                     │
│read_data      ││ 00000000           │48656C6C                      │
│               ││────────────────────┴─────────                     │
└───────────────┘└───────────────────────────────────────────────────┘
  |}]
;;
