open Base
open Hardcaml

module Make_StencilSIMD (Config : sig
    val data_width : int
    val simd_width : int
  end) =
struct
  module StencilLane = struct
    type 'a t =
      { nw : 'a [@bits Config.data_width]
      ; no : 'a [@bits Config.data_width]
      ; ne : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; boot : 'a
      ; ccurr : 'a StencilLane.t array [@length Config.simd_width]
      ; hit_range : 'a array [@length Config.simd_width] [@bits 3]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { cnext : 'a array [@length Config.simd_width] [@bits Config.data_width]
      ; overflow : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module Stencil =
      Stencil.Make_Stencil (struct
        let data_width = Config.data_width
      end)
    in
    let open Signal in
    let stencils =
      List.init Config.simd_width ~f:(fun n ->
        Stencil.hierarchical
          scope
          { clock = i.clock
          ; clear = i.clear
          ; enable = i.enable
          ; boot = i.boot
          ; hit_range = i.hit_range.(n)
          ; nw = i.ccurr.(n).nw
          ; no = i.ccurr.(n).no
          ; ne = i.ccurr.(n).ne
          })
    in
    { cnext = List.map stencils ~f:(fun s -> s.value) |> List.to_array
    ; overflow = List.map stencils ~f:(fun s -> s.overflow) |> List.reduce_exn ~f:( |: )
    ; ready = List.map stencils ~f:(fun s -> s.ready) |> List.reduce_exn ~f:( |: )
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"stencil_simd" create input
  ;;
end
