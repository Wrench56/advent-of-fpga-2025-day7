open Base
open Hardcaml

module Make_Counter (Config : sig
    val max_num : int
    val saturating : bool
  end) =
struct
  let max_num = Int.max 1 Config.max_num
  let data_width = max_num + 1 |> Int.ceil_log2

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; increment : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { count : 'a [@bits data_width] } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Hardcaml.Signal in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let maxv = max_num |> of_int ~width:data_width in
    let counter =
      reg_fb spec ~enable:i.increment ~width:data_width ~f:(fun c ->
        if Config.saturating then mux2 (c ==: maxv) c (c +:. 1) else c +:. 1)
    in
    { O.count = counter }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"counter" create input
  ;;
end
