open Base
open Hardcaml
open Solution.Stencil_simd

let testbench_fuzz () =
  let data_width = 16 in
  let simd_width = 4 in
  let scope = Scope.create ~flatten_design:true () in
  let module TestStencilSIMD =
    Make_StencilSIMD (struct
      let data_width = data_width
      let simd_width = simd_width
    end)
  in
  let module Sim = Cyclesim.With_interface (TestStencilSIMD.I) (TestStencilSIMD.O) in
  let sim = Sim.create (TestStencilSIMD.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle n = Utils.cycle sim n in
  let enable () = inputs.enable := Bits.vdd in
  let disable () = inputs.enable := Bits.gnd in
  let clear () =
    inputs.clear := Bits.vdd;
    cycle 1;
    inputs.clear := Bits.gnd
  in
  let drive_simd hitl (ccurrl : int TestStencilSIMD.StencilLane.t list) =
    assert (List.length hitl = simd_width);
    assert (List.length ccurrl = simd_width);
    List.iteri hitl ~f:(fun i hit ->
      inputs.hit_range.(i) := List.rev hit |> List.map ~f:Bool.to_int |> Bits.of_bit_list);
    List.iteri ccurrl ~f:(fun i { nw; no; ne } ->
      let lane = inputs.ccurr.(i) in
      lane.nw := Bits.of_int ~width:data_width nw;
      lane.no := Bits.of_int ~width:data_width no;
      lane.ne := Bits.of_int ~width:data_width ne);
    cycle 1
  in
  let verif_print
        (inputs : Bits.t ref TestStencilSIMD.I.t)
        (outputs : Bits.t ref TestStencilSIMD.O.t)
    =
    let () = Stdio.printf "Enabled  | Ready | OVF   |\n" in
    let () =
      Stdio.printf
        "%5b    | %5b | %5b |\n"
        (Bits.to_bool !(inputs.enable))
        (Bits.to_bool !(outputs.ready))
        (Bits.to_bool !(outputs.overflow))
    in
    let () = Stdio.printf "Lane     | NW    | NO    | NE    | HIT | OUTPUT |\n" in
    for i = 0 to simd_width - 1 do
      Stdio.printf
        "Lane%4d | %5d | %5d | %5d | %d%d%d | %6d |\n"
        i
        (Bits.to_int !(inputs.ccurr.(i).nw))
        (Bits.to_int !(inputs.ccurr.(i).no))
        (Bits.to_int !(inputs.ccurr.(i).ne))
        (Bits.to_int (Bits.bit !(inputs.hit_range.(i)) 0))
        (Bits.to_int (Bits.bit !(inputs.hit_range.(i)) 1))
        (Bits.to_int (Bits.bit !(inputs.hit_range.(i)) 2))
        (Bits.to_int !(outputs.cnext.(i)))
    done;
    Stdio.printf "=================================================\n\n"
  in
  let maxval = Int.pow 2 data_width - 1 in
  let make2dcopy lst times = List.init times ~f:(fun _ -> lst) in
  let hit_max = make2dcopy [ true; false; true ] simd_width in
  let hit_min = make2dcopy [ false; true; false ] simd_width in
  let open TestStencilSIMD.StencilLane in
  let val_max = make2dcopy { nw = maxval; no = maxval; ne = maxval } simd_width in
  let val_spl = make2dcopy { nw = 1; no = 2; ne = 3 } simd_width in
  clear ();
  disable ();
  drive_simd hit_max val_max;
  verif_print inputs outputs;
  enable ();
  cycle 1;
  verif_print inputs outputs;
  drive_simd hit_max val_spl;
  verif_print inputs outputs;
  drive_simd hit_min val_max;
  verif_print inputs outputs;
  drive_simd
    [ [ true; true; false ]
    ; [ false; true; true ]
    ; [ true; false; true ]
    ; [ true; false; false ]
    ]
    [ { nw = 1; no = 2; ne = 3 }
    ; { nw = 102; no = 10; ne = 1 }
    ; { nw = 52780; no = 60937; ne = 43694 }
    ; { nw = 1203; no = 1020; ne = 0 }
    ];
  verif_print inputs outputs
;;

let%expect_test "Stencil SIMD works as expected" =
  let () = testbench_fuzz () in
  [%expect
    {|
Enabled  | Ready | OVF   |
false    | false | false |
Lane     | NW    | NO    | NE    | HIT | OUTPUT |
Lane   0 | 65535 | 65535 | 65535 | 101 |      0 |
Lane   1 | 65535 | 65535 | 65535 | 101 |      0 |
Lane   2 | 65535 | 65535 | 65535 | 101 |      0 |
Lane   3 | 65535 | 65535 | 65535 | 101 |      0 |
=================================================

Enabled  | Ready | OVF   |
 true    |  true |  true |
Lane     | NW    | NO    | NE    | HIT | OUTPUT |
Lane   0 | 65535 | 65535 | 65535 | 101 |  65533 |
Lane   1 | 65535 | 65535 | 65535 | 101 |  65533 |
Lane   2 | 65535 | 65535 | 65535 | 101 |  65533 |
Lane   3 | 65535 | 65535 | 65535 | 101 |  65533 |
=================================================

Enabled  | Ready | OVF   |
 true    |  true | false |
Lane     | NW    | NO    | NE    | HIT | OUTPUT |
Lane   0 |     1 |     2 |     3 | 101 |      6 |
Lane   1 |     1 |     2 |     3 | 101 |      6 |
Lane   2 |     1 |     2 |     3 | 101 |      6 |
Lane   3 |     1 |     2 |     3 | 101 |      6 |
=================================================

Enabled  | Ready | OVF   |
 true    |  true | false |
Lane     | NW    | NO    | NE    | HIT | OUTPUT |
Lane   0 | 65535 | 65535 | 65535 | 010 |      0 |
Lane   1 | 65535 | 65535 | 65535 | 010 |      0 |
Lane   2 | 65535 | 65535 | 65535 | 010 |      0 |
Lane   3 | 65535 | 65535 | 65535 | 010 |      0 |
=================================================

Enabled  | Ready | OVF   |
 true    |  true |  true |
Lane     | NW    | NO    | NE    | HIT | OUTPUT |
Lane   0 |     1 |     2 |     3 | 110 |      1 |
Lane   1 |   102 |    10 |     1 | 011 |      1 |
Lane   2 | 52780 | 60937 | 43694 | 101 |  26339 |
Lane   3 |  1203 |  1020 |     0 | 100 |   2223 |
=================================================
|}]
;;
