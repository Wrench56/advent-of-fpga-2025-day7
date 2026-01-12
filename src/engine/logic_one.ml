open Base
open Hardcaml
open Common

module Make_LogicOne (Config : sig
    val data_width : int
    val data_depth : int
    val max_value : int
    val mem_fetch_delay : int
  end) =
struct
  let addr_width = Int.ceil_log2 Config.data_depth
  let count_size = Int.max 1 Config.max_value + 1 |> Int.ceil_log2

  module LogicState = struct
    type t =
      | Boot
      | FetchBeams
      | FetchBeamsWait
      | FetchSplitters
      | FetchSplittersWait
      | ExecLogic
      | Popcount
      | PopcountWait
      | AccuWait
      | Finished
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; mem_data : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { solution_ready : 'a
      ; solution : 'a [@bits count_size]
      ; mem_addr : 'a [@bits addr_width]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module SLatch =
      Slatch.Make_SLatch (struct
        let data_width = Config.data_width
        let data_depth = Config.data_depth
        let mem_fetch_delay = Config.mem_fetch_delay
      end)
    in
    let module PopCntr =
      Popcntr.Make_Popcounter (struct
        let data_width = Config.data_width
      end)
    in
    let module PPB =
      Pingpong.Make_PingPongBuffer (struct
        let data_width = Config.data_width
      end)
    in
    let module Cntr =
      Counter.Make_Counter (struct
        let max_num = Config.data_depth - 1
        let saturating = true
      end)
    in
    let module Accu =
      Accumulator.Make_Accumulator (struct
        let max_value = Config.max_value
        let add_width = Config.data_width + 1 |> Int.ceil_log2
        let saturating = true
      end)
    in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
    let%hw_var slatch_next_cycle = Always.Variable.wire ~default:Signal.gnd in
    let incr_internal_cntr = Always.Variable.wire ~default:Signal.gnd in
    let%hw_var automata_finished = Always.Variable.wire ~default:Signal.gnd in
    let solution_ready = Always.Variable.wire ~default:Signal.gnd in
    let%hw_var swap_imp = Always.Variable.wire ~default:Signal.gnd in
    let%hw_var popcount_finished = Always.Variable.wire ~default:Signal.gnd in
    let hit_reg = Always.Variable.reg ~width:Config.data_width spec in
    let beams_next = Always.Variable.wire ~default:(Signal.zero Config.data_width) in
    let state = Always.State_machine.create (module LogicState) spec in
    let internal_cntr =
      Cntr.hierarchical
        scope
        { clock = i.clock; clear = i.reset; increment = incr_internal_cntr.value }
    in
    let slatch =
      SLatch.hierarchical
        scope
        { clock = i.clock
        ; reset = i.reset
        ; next = slatch_next_cycle.value
        ; input = i.mem_data
        }
    in
    let ppb =
      PPB.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; enable = Signal.vdd
        ; swap = swap_imp.value
        ; input = beams_next.value
        }
    in
    let popcntr =
      PopCntr.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; enable = automata_finished.value
        ; data = hit_reg.value
        }
    in
    let accu =
      Accu.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; enable = popcount_finished.value
        ; add = popcntr.count
        }
    in
    let open Always in
    let open Signal in
    (* TODO: Clean this up using flag registers and remove unneeded states *)
    Always.compile
      [ state.switch
          [ Boot, [ state.set_next FetchBeams ]
          ; ( FetchBeams
            , [ slatch_next_cycle <--. 1
              ; incr_internal_cntr <--. 1
              ; state.set_next FetchBeamsWait
              ] )
          ; ( FetchBeamsWait
            , [ when_
                  slatch.ready
                  [ beams_next <-- slatch.output
                  ; swap_imp <--. 1
                  ; state.set_next FetchSplitters
                  ]
              ] )
          ; ( FetchSplitters
            , [ if_
                  (internal_cntr.count <:. Config.data_depth - 1)
                  [ slatch_next_cycle <--. 1
                  ; incr_internal_cntr <--. 1
                  ; state.set_next FetchSplittersWait
                  ]
                  [ state.set_next Finished ]
              ] )
          ; FetchSplittersWait, [ when_ slatch.ready [ state.set_next ExecLogic ] ]
          ; ( ExecLogic
            , [ hit_reg <-- (ppb.output &: slatch.output)
              ; (beams_next
                 <--
                 let hit_splitters = ppb.output &: slatch.output in
                 let unhit = ppb.output &: ~:hit_splitters in
                 unhit |: sll hit_splitters 1 |: srl hit_splitters 1)
              ; swap_imp <--. 1
              ; state.set_next Popcount
              ] )
          ; Popcount, [ automata_finished <--. 1; state.set_next PopcountWait ]
          ; ( PopcountWait
            , [ automata_finished <--. 1
              ; when_ popcntr.ready [ popcount_finished <--. 1; state.set_next AccuWait ]
              ] )
          ; ( AccuWait
            , [ popcount_finished <--. 1
              ; when_ accu.ready [ state.set_next FetchSplitters ]
              ] )
          ; Finished, [ solution_ready <--. 1 ]
          ]
      ];
    { O.mem_addr = slatch.addr
    ; solution_ready = solution_ready.value
    ; solution = accu.sum
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"logic_one" create input
  ;;
end
