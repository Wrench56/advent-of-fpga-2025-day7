open Base
open Hardcaml

module Make_Accumulator (Config : sig
    val max_value : int
    val add_width : int
    val saturating : bool
  end) =
struct
  let max_value = Int.max 1 Config.max_value
  let reg_size = max_value + 1 |> Int.ceil_log2

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; add : 'a [@bits Config.add_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { sum : 'a [@bits reg_size]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Hardcaml.Signal in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let maxv = max_value |> of_int ~width:reg_size in
    let add_ext = uresize i.add reg_size in
    let acc =
      reg_fb spec ~enable:i.enable ~width:reg_size ~f:(fun acc ->
        let wide_next = uresize acc (reg_size + 1) +: uresize add_ext (reg_size + 1) in
        let maxv_wide = uresize maxv (reg_size + 1) in
        if Config.saturating
        then mux2 (wide_next >: maxv_wide) maxv (lsbs wide_next)
        else lsbs wide_next)
    in
    (* Ready signal is unused for now. Use if latency ever increases *)
    { O.sum = acc; ready = Signal.vdd }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"accumulator" create input
  ;;
end
