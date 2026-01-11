open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Pingpongram

let testbench () =
  let data_width = 16 in
  let data_depth = 8 in
  let fetch_delay = 2 in
  let write_delay = 2 in
  let addr_width = Int.ceil_log2 data_depth in
  let scope = Scope.create ~flatten_design:true () in
  let module TestPingPongRAM =
    Make_PingPongRAM (struct
      let data_width = data_width
      let data_depth = data_depth
      let mem_fetch_delay = fetch_delay
      let mem_write_delay = write_delay
    end)
  in
  let module Sim = Cyclesim.With_interface (TestPingPongRAM.I) (TestPingPongRAM.O) in
  let sim = Sim.create (TestPingPongRAM.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let set_buffer_at addr value =
    inputs.write_addr := Bits.of_int ~width:addr_width addr;
    inputs.input := Bits.of_int ~width:data_width value
  in
  let read_buffer_at addr = inputs.read_addr := Bits.of_int ~width:addr_width addr in
  let set_zero () = inputs.input := Bits.of_int ~width:data_width 0 in
  let write_req state = inputs.write_req := Bits.of_bool state in
  let read_req state = inputs.read_req := Bits.of_bool state in
  let clear () =
    set_zero ();
    inputs.clear := Bits.vdd;
    cycle 1;
    inputs.clear := Bits.gnd
  in
  let swp_buffer () =
    inputs.swap := Bits.vdd;
    cycle 1;
    inputs.swap := Bits.gnd
  in
  write_req false;
  read_req false;
  clear ();
  set_buffer_at 0 0xDEAD;
  cycle 1;
  write_req true;
  read_buffer_at 0;
  cycle 1;
  write_req false;
  cycle 1;
  write_req true;
  cycle 1;
  write_req false;
  swp_buffer ();
  read_req true;
  cycle 3;
  read_req false;
  swp_buffer ();
  write_req true;
  set_buffer_at 1 0xBEEF;
  cycle 3;
  write_req false;
  swp_buffer ();
  read_buffer_at 1;
  read_req true;
  cycle 4;
  waves
;;

let%expect_test "Test RAM-based PingPongBuffer" =
  let waves = testbench () in
  Waveform.print
    ~wave_width:2
    ~display_width:130
    ~display_height:28
    ~display_rules:
      [ Display_rule.port_name_is_one_of
          [ "clock"
          ; "clear"
          ; "swap"
          ; "read_req"
          ; "write_req"
          ; "read_ready"
          ; "write_ready"
          ]
          ~wave_format:Wave_format.Bit
      ; Display_rule.port_name_is_one_of
          [ "input"; "output" ]
          ~wave_format:Wave_format.Hex
      ; Display_rule.port_name_matches (Re.Posix.compile (Re.Posix.re ".*"))
      ]
    waves;
  [%expect
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
│clear             ││──────┐                                                                                                     │
│                  ││      └─────────────────────────────────────────────────────────────────────────────────────────────────────│
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
│read_req          ││                                    ┌─────────────────┐                             ┌───────────────────────│
│                  ││────────────────────────────────────┘                 └─────────────────────────────┘                       │
│swap              ││                              ┌─────┐                 ┌─────┐                 ┌─────┐                       │
│                  ││──────────────────────────────┘     └─────────────────┘     └─────────────────┘     └───────────────────────│
│write_req         ││            ┌─────┐     ┌─────┐                             ┌─────────────────┐                             │
│                  ││────────────┘     └─────┘     └─────────────────────────────┘                 └─────────────────────────────│
│read_ready        ││──────────────────────────────────────────┐           ┌───────────────────────────────────┐           ┌─────│
│                  ││                                          └───────────┘                                   └───────────┘     │
│write_ready       ││──────────────────┐           ┌───────────────────────────────────┐           ┌─────────────────────────────│
│                  ││                  └───────────┘                                   └───────────┘                             │
│                  ││──────┬─────────────────────────────────────────────────────┬───────────────────────────────────────────────│
│input             ││ 0000 │DEAD                                                 │BEEF                                           │
│                  ││──────┴─────────────────────────────────────────────────────┴───────────────────────────────────────────────│
│                  ││──────────────────────────────────────────┬───────────────────────────────────────────────┬─────────────────│
│output            ││ 0000                                     │DEAD                                           │BEEF             │
│                  ││──────────────────────────────────────────┴───────────────────────────────────────────────┴─────────────────│
│                  ││────────────────────────────────────────────────────────────────────────────────────┬───────────────────────│
│read_addr         ││ 000                                                                                │001                    │
│                  ││────────────────────────────────────────────────────────────────────────────────────┴───────────────────────│
│                  ││────────────────────────────────────────────────────────────┬───────────────────────────────────────────────│
│write_addr        ││ 000                                                        │001                                            │
│                  ││────────────────────────────────────────────────────────────┴───────────────────────────────────────────────│
└──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
|}]
;;
