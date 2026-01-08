open Base
open Hardcaml

module Make_Popcounter (Config : sig
    val data_width : int
  end) =
struct
  let cntr_width = Config.data_width + 1 |> Int.ceil_log2

  module I = struct
    (* TODO: Implement sequential popcounter for huge data_width (that is why I am keeping [clock] and [clear]) *)
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; data : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { count : 'a [@bits cntr_width]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Signal in
    { O.count = mux2 i.enable (uresize (popcount i.data) cntr_width) (zero cntr_width)
    ; ready = vdd &: i.enable
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"popcounter" create input
  ;;
end
