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

  let is_pow2_minus1 x =
    let open Int in
    x >= 1 && (x + 1) land x = 0
  ;;

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
    let maxv = of_int ~width:reg_size max_value in
    let add_ext = uresize i.add reg_size in
    let wide_size = reg_size + 1 in
    let busy =
      reg_fb spec ~enable:vdd ~width:1 ~f:(fun busy ->
        let idle = busy ==:. 0 in
        idle &: i.enable)
    in
    let idle = busy ==:. 0 in
    let stage1_active = idle &: i.enable in
    let stage2_active = busy in
    (*
    Stage 1: Add the [add] value to the accumulator sum
    Stage 2: Write back and saturation logic
     *)
    let acc =
      reg_fb spec ~enable:vdd ~width:reg_size ~f:(fun acc_q ->
        let wide_next = uresize acc_q wide_size +: uresize add_ext wide_size in
        let wide_q = reg spec ~enable:stage1_active wide_next in
        let acc_next_from_wide =
          (*
            Given the [max_value] consists of only set bits (e.g. 1023),
            it is trivial to check whether an overflow happened by checking
            the extra "wide" bit. If it is set, the accumulator overflowed
            and no extra logic is needed. For regular numbers (e.g. 103), we
            still need to do a comparison.
          *)
          if Config.saturating && is_pow2_minus1 max_value
          then (
            let sum = lsbs wide_q in
            let overflow = msb wide_q in
            let all_ones = ones reg_size in
            mux2 overflow all_ones sum)
          else (
            let maxv_wide = uresize maxv wide_size in
            if Config.saturating
            then mux2 (wide_q >: maxv_wide) maxv (lsbs wide_q)
            else lsbs wide_q)
        in
        mux2 stage2_active acc_next_from_wide acc_q)
    in
    let ready = idle in
    { O.sum = acc; ready }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"accumulator" create input
  ;;
end
