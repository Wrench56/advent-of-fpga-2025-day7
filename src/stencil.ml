open Base
open Hardcaml

module Make_Stencil (Config : sig
    val data_width : int
  end) =
struct
  module I = struct
    (* The 3 data inputs ([nw], [no], [ne]) represent North-West, North, and North-East from Moore neightborhood. *)
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; hit_range : 'a [@bits 3]
      ; nw : 'a [@bits Config.data_width]
      ; no : 'a [@bits Config.data_width]
      ; ne : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { cell : 'a [@bits Config.data_width]
      ; overflow : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module Adder3 =
      Adder.Make_AdderN (struct
        let num_inputs = 3
        let add_width = Config.data_width
      end)
    in
    let open Signal in
    let zero_sig = zero Config.data_width in
    let adder =
      Adder3.hierarchical
        scope
        { clock = i.clock
        ; enable = i.enable
        ; clear = i.clear
        ; nums =
            [| mux2 i.hit_range.:[0, 0] i.nw zero_sig
             ; mux2 i.hit_range.:[1, 1] zero_sig i.no
             ; mux2 i.hit_range.:[2, 2] i.ne zero_sig
            |]
        }
    in
    let open Signal in
    { cell = adder.sum
    ; overflow =
        mux2 (adder.carry >: zero (Signal.width adder.carry)) Signal.vdd Signal.gnd
    ; ready = adder.ready
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"stencil" create input
  ;;
end
