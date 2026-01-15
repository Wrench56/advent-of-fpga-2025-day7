open Base
open Hardcaml
open Common

module Make_BeamEngine (Config : sig
    val data_width : int
    val data_depth : int
    val max_value : int
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
      ; mem_ready : 'a
      ; mem_data : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { next_iter_ready : 'a
      ; hit_splitters_ready : 'a
      ; hit_splitters : 'a [@bits Config.data_width]
      ; solution_ready : 'a
      ; solution : 'a [@bits count_size]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
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
    let boot_mode_done = Always.Variable.reg ~width:1 spec in
    let beams_next = Always.Variable.wire ~default:(Signal.zero Config.data_width) in
    let hit_splitters_ready_d = Always.Variable.wire ~default:Signal.gnd in
    let state = Always.State_machine.create (module LogicState) spec in
    let internal_cntr =
      Cntr.hierarchical
        scope
        { clock = i.clock; clear = i.reset; increment = incr_internal_cntr.value }
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
                  i.mem_ready
                  [ beams_next <-- i.mem_data
                  ; if_
                      ~:(boot_mode_done.value)
                      [ hit_reg <-- i.mem_data
                      ; hit_splitters_ready_d <--. 1
                      ; boot_mode_done <--. 1
                      ]
                      []
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
          ; FetchSplittersWait, [ when_ i.mem_ready [ state.set_next ExecLogic ] ]
          ; ( ExecLogic
            , let hit_splitters = ppb.output &: i.mem_data in
              [ hit_reg <-- hit_splitters
              ; (beams_next
                 <--
                 let shl_hit_spl = sll hit_splitters 1 in
                 let shr_hit_spl = srl hit_splitters 1 in
                 let unhit = ppb.output &: ~:hit_splitters in
                 unhit |: shl_hit_spl |: shr_hit_spl)
              ; hit_splitters_ready_d <--. 1
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
    { next_iter_ready = slatch_next_cycle.value
    ; hit_splitters_ready = Signal.pipeline ~n:1 spec hit_splitters_ready_d.value
    ; hit_splitters = hit_reg.value
    ; solution_ready = solution_ready.value
    ; solution = accu.sum
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"beam_engine" create input
  ;;
end
