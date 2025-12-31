open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Slatch

let testbench () =
  let data_width = 8 in
  let data_depth = 8 in
  let mem_fetch_delay = 2 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestSLatch =
    Make_SLatch (struct
      let data_width = data_width
      let data_depth = data_depth
      let mem_fetch_delay = mem_fetch_delay
    end)
  in
  let module Sim = Cyclesim.With_interface (TestSLatch.I) (TestSLatch.O) in
  let sim = Sim.create (TestSLatch.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let memory = Array.init data_depth ~f:(fun e -> e * 32) in
  let addresses = Queue.create () in
  (* Simulate memory fetch delay *)
  for _ = 1 to mem_fetch_delay do
    Queue.enqueue addresses 0
  done;
  let set_next () = inputs.next := Bits.vdd in
  let clr_next () = inputs.next := Bits.gnd in
  let cycle n =
    for _ = 1 to n do
      let addr = Bits.to_int !(outputs.addr) in
      Queue.enqueue addresses addr;
      let delayed_addr = Queue.dequeue_exn addresses in
      inputs.input := memory.(delayed_addr) |> Bits.of_int ~width:data_width;
      Utils.cycle sim 1
    done
  in
  let reset () =
    inputs.reset := Bits.vdd;
    clr_next ();
    cycle 1;
    inputs.reset := Bits.gnd
  in
  reset ();
  set_next ();
  cycle 1;
  clr_next ();
  cycle 1;
  set_next ();
  cycle 1;
  clr_next ();
  cycle 2;
  set_next ();
  cycle 7;
  waves
;;

let%expect_test "[S]plitter Latch works as expected" =
  let waves = testbench () in
  Waveform.print ~wave_width:2 ~display_width:100 waves;
  [%expect
    {|
┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
│clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
│                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
│reset             ││                                                                              │
│                  ││──────────────────────────────────────────────────────────────────────────────│
│next              ││      ┌─────┐     ┌─────┐           ┌─────────────────────────────────────────│
│                  ││──────┘     └─────┘     └───────────┘                                         │
│                  ││────────────────────────┬─────────────────────────────┬───────────────────────│
│read_data         ││ 00                     │20                           │40                     │
│                  ││────────────────────────┴─────────────────────────────┴───────────────────────│
│                  ││────────────┬─────────────────────────────┬───────────────────────┬───────────│
│addr              ││ 0          │1                            │2                      │3          │
│                  ││────────────┴─────────────────────────────┴───────────────────────┴───────────│
│ready             ││                        ┌─────┐                       ┌─────┐                 │
│                  ││────────────────────────┘     └───────────────────────┘     └─────────────────│
│                  ││──────────────────────────────┬─────────────────────────────┬─────────────────│
│write_data        ││ 00                           │20                           │40               │
│                  ││──────────────────────────────┴─────────────────────────────┴─────────────────│
│                  ││                                                                              │
└──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
|}]
;;
