open Base
open Hardcaml
open Hardcaml_waveterm
open Solution.Common.Rom

let testbench data latency =
  let scope = Scope.create ~flatten_design:true () in
  let module TestROM =
    Make_ROM (struct
      let rom_content = data
      let latency = latency
    end)
  in
  let module Sim = Cyclesim.With_interface (TestROM.I) (TestROM.O) in
  let sim = Sim.create (TestROM.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let auto_cycle () =
    for
      _ = 1
      to match latency with
         | None -> 1
         | Some latency -> Int.max 1 latency
    do
      Utils.cycle sim 1
    done
  in
  let enable () = inputs.enable := Bits.vdd in
  let disable () = inputs.enable := Bits.gnd in
  let data_width = List.hd_exn data |> List.length in
  let addr_width = List.length data |> Int.ceil_log2 in
  let set_read addr = inputs.read_addr := Bits.of_int ~width:addr_width addr in
  let test word iter =
    let is_enabled = Bits.equal !(inputs.enable) Bits.vdd in
    let expected =
      if is_enabled
      then List.map word ~f:Bits.of_bool |> Array.of_list |> Bits.of_array
      else Bits.zero data_width
    in
    let actual = !(outputs.data) in
    if not (Bits.equal expected actual)
    then
      Stdio.printf
        "Expected (%s) != Actual (%s) for word %i while ROM was %s\n"
        (Bits.to_string expected)
        (Bits.to_string actual)
        iter
        (if is_enabled then "enabled" else "disabled")
  in
  List.iteri data ~f:(fun i word ->
    set_read i;
    enable ();
    auto_cycle ();
    test word i;
    disable ();
    auto_cycle ();
    test word i);
  Stdio.printf "All tests ran.";
  waves
;;

let romc_of_str lst =
  List.map lst ~f:(fun word ->
    Bits.of_string word
    |> Bits.to_array
    |> Array.to_list
    |> List.map ~f:(fun bit -> Bits.to_bool bit))
;;

let%expect_test "Combinational Read-Only Memory works as expected" =
  let _waves =
    testbench
      (romc_of_str
         [ "8'b10101010"
         ; "8'b00000000"
         ; "8'b11111111"
         ; "8'b00011000"
         ; "8'b10000001"
         ; "8'b10001000"
         ])
      None
  in
  [%expect
    {|
All tests ran.
|}]
;;

let%expect_test "Read-Only Memory with 2-cycle latency works as expected" =
  let _waves =
    testbench
      (romc_of_str
         [ "8'b11111111"
         ; "8'b00000001"
         ; "8'b11111110"
         ; "8'b10011000"
         ; "8'b11000011"
         ; "8'b10011001"
         ])
      (Some 2)
  in
  [%expect
    {|
All tests ran.
|}]
;;
