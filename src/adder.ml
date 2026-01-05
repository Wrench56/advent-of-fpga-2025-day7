open Base
open Hardcaml

module Make_AdderN (Config : sig
    val num_inputs : int
    val add_width : int
  end) =
struct
  assert (Config.num_inputs > 1);;

  module I = struct
    (* Im using arrays because of their Cyclesim addressing simplicity *)
    (* TODO: Implement sequential popcounter for huge data_width (that is why I am keeping [clock] and [clear]) *)
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; nums : 'a array [@length Config.num_inputs] [@bits Config.add_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { sum : 'a [@bits Config.add_width]
      ; carry : 'a [@bits Int.ceil_log2 Config.num_inputs]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Signal in
    let cwidth = Config.add_width + Int.ceil_log2 Config.num_inputs in
    let carry_and_sum =
      mux2
        i.enable
        (i.nums
         |> Array.map ~f:(fun v -> uresize v cwidth)
         |> Array.to_list
         |> tree ~arity:2 ~f:(fun s -> uresize (reduce ~f:( +: ) s) cwidth))
        (zero cwidth)
    in
    { sum = carry_and_sum.:[Config.add_width - 1, 0]
    ; carry = carry_and_sum.:[cwidth - 1, Config.add_width]
    ; ready = i.enable
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:("adder_" ^ Int.to_string Config.num_inputs) create input
  ;;
end
