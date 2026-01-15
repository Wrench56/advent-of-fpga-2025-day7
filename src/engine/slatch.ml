open Base
open Hardcaml
open Common

module Make_SLatch (Config : sig
    val data_width : int
    val data_depth : int
    val mem_fetch_delay : int
  end) =
struct
  let addr_width = Int.ceil_log2 Config.data_depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; step : 'a
      ; data_in : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data_out : 'a [@bits Config.data_width]
      ; addr : 'a [@bits addr_width]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module AddrCntr =
      Counter.Make_Counter (struct
        let max_num = Config.data_depth - 1
        let saturating = true
      end)
    in
    let open Signal in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
    (*
       [accept] is only true iff a request was sent through [i.step] and we are not [busy].
       Once we accept a fetch request, we start a [Config.mem_fetch_delay] pulse delay so that the memory
       can set the valid address and fetch the S (as in "Splitter" from the AoC problem) block.
       [rdy_pulse] will signal when this delay has expired.
       [busy] flag is a register that is kept in busy state iff either [busy] or [accept] is true
       (the latter is needed for initial set of the busy flag) AND we have not received a [rdy_pulse]
       (this is needed to reset the busy flag)
    *)
    let%hw busy_d = wire 1 in
    let%hw busy = reg spec ~enable:Signal.vdd busy_d in
    let%hw accept = i.step &: ~:busy in
    let%hw boot_mode_done =
      reg_fb spec ~enable:Signal.vdd ~width:1 ~f:(fun prev -> prev |: accept)
    in
    let%hw rdy_pulse =
      pipeline spec ~enable:Signal.vdd ~n:(Config.mem_fetch_delay + 1) accept
    in
    Signal.assign busy_d (busy |: accept &: ~:rdy_pulse);
    let addrcntr =
      AddrCntr.hierarchical
        scope
        { AddrCntr.I.clock = i.clock
        ; clear = i.reset
        ; increment = accept &: boot_mode_done
        }
    in
    let%hw latch = reg spec ~enable:rdy_pulse i.data_in in
    let%hw data_rdy = pipeline spec ~enable:Signal.vdd ~n:1 rdy_pulse in
    { data_out = latch; addr = addrcntr.count; ready = data_rdy }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"s_latch" create input
  ;;
end
