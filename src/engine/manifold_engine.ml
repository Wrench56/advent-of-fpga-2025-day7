open Base
open Hardcaml
open Common.Accumulator
open Common.Adder
open Common.Counter
open Common.Pingpongram
open Stencilblocks.Stencil_simd

module Make_ManifoldEngine (Config : sig
    val data_width : int
    val data_depth : int
    val simd_cell_width : int
    val simd_width : int
    val max_value : int
    val mem_fetch_delay : int
    val mem_write_delay : int
  end) =
struct
  let count_width = Int.max 1 Config.max_value + 1 |> Int.ceil_log2

  let iter_per_lane =
    if Config.data_width % Config.simd_width <> 0
    then failwith "[data_width] is not multiple of [simd_width]"
    else Config.data_width / Config.simd_width
  ;;

  let addr_width = Int.ceil_log2 iter_per_lane

  module HitSplitterLane = struct
    type 'a t =
      { shl : 'a [@bits Config.data_width]
      ; reg : 'a [@bits Config.data_width]
      ; shr : 'a [@bits Config.data_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module LogicState = struct
    type t =
      | Boot
      | SimdWaitRead
      | SimdWaitData
      | SimdExecute
      | SimdWaitSimd
      | SimdWaitWrite
      | SimdFinished
      | AdderRead
      | AdderWaitRead
      | AdderExecute
      | AdderWaitAccumulator
      | Finished
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; enable : 'a
      ; sim_ready : 'a
      ; hit_splitters : 'a HitSplitterLane.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { next_iter_ready : 'a
      ; solution_ready : 'a
      ; overflow : 'a
      ; solution : 'a [@bits count_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module StencilSIMD =
      Make_StencilSIMD (struct
        let data_width = Config.simd_cell_width
        let simd_width = Config.simd_width
      end)
    in
    let module PingPongRAM =
      Make_PingPongRAM (struct
        let data_width = Config.simd_cell_width * Config.simd_width
        let data_depth = iter_per_lane
        let mem_fetch_delay = Config.mem_fetch_delay
        let mem_write_delay = Config.mem_write_delay
      end)
    in
    let module RowCounter =
      Make_Counter (struct
        let max_num = Config.data_depth - 1
        let saturating = true
      end)
    in
    let module AdderSIMD =
      Make_AdderN (struct
        let num_inputs = Config.simd_width
        let add_width = count_width
      end)
    in
    let module SolutionAccumulator =
      Make_Accumulator (struct
        let max_value = Config.max_value
        let add_width = count_width
        let saturating = true
      end)
    in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
    let state = Always.State_machine.create (module LogicState) spec in
    let boot_d = Always.Variable.reg spec ~width:1 in
    let simd_ccurr =
      Array.init Config.simd_width ~f:(fun _ ->
        { StencilSIMD.StencilLane.nw =
            Always.Variable.reg spec ~width:Config.simd_cell_width
        ; no = Always.Variable.reg spec ~width:Config.simd_cell_width
        ; ne = Always.Variable.reg spec ~width:Config.simd_cell_width
        })
    in
    let simd_hit_range =
      Array.init Config.simd_width ~f:(fun _ -> Always.Variable.reg spec ~width:3)
    in
    let simd =
      StencilSIMD.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; enable = i.enable
        ; boot = boot_d.value
        ; ccurr =
            Array.map simd_ccurr ~f:(fun lane ->
              { StencilSIMD.StencilLane.nw = lane.nw.value
              ; no = lane.no.value
              ; ne = lane.ne.value
              })
        ; hit_range = Array.map simd_hit_range ~f:(fun hit_range -> hit_range.value)
        }
    in
    let write_req_d = Always.Variable.wire ~default:Signal.gnd in
    let read_req_d = Always.Variable.wire ~default:Signal.gnd in
    let swap_d = Always.Variable.wire ~default:Signal.gnd in
    let next_iter_ready_d = Always.Variable.wire ~default:Signal.gnd in
    let ppb_write_d =
      Always.Variable.wire
        ~default:(Signal.zero (Config.simd_cell_width * Config.simd_width))
    in
    let%hw_var read_addr = Always.Variable.reg spec ~width:addr_width in
    let%hw_var write_addr = Always.Variable.reg spec ~width:addr_width in
    let ppb =
      PingPongRAM.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; swap = swap_d.value
        ; write_req = write_req_d.value
        ; read_req = read_req_d.value
        ; read_addr = read_addr.value
        ; write_addr = write_addr.value
        ; input = ppb_write_d.value
        }
    in
    let iter = Always.Variable.reg spec ~width:addr_width in
    let incr_d = Always.Variable.wire ~default:Signal.gnd in
    let solution_ready_d = Always.Variable.wire ~default:Signal.gnd in
    let row_cntr =
      RowCounter.hierarchical
        scope
        { clock = i.clock; clear = i.reset; increment = incr_d.value }
    in
    let lane_high_bit lane = ((Config.simd_width - lane) * Config.simd_cell_width) - 1 in
    let lane_low_bit lane = lane_high_bit lane - Config.simd_cell_width + 1 in
    let adder_en_d = Always.Variable.wire ~default:Signal.gnd in
    let adder_simd =
      AdderSIMD.hierarchical
        scope
        { clock = i.clock
        ; enable = adder_en_d.value
        ; clear = i.reset
        ; nums =
            (let open Signal in
             Array.init Config.simd_width ~f:(fun lane ->
               uresize ppb.output.:[lane_high_bit lane, lane_low_bit lane] count_width))
        }
    in
    let accu_en_d = Always.Variable.wire ~default:Signal.gnd in
    let sol_accu =
      SolutionAccumulator.hierarchical
        scope
        { clock = i.clock
        ; clear = i.reset
        ; enable = accu_en_d.value
        ; add = adder_simd.sum
        }
    in
    let open Always in
    let open Signal in
    let windows_of_row row =
      Array.init iter_per_lane ~f:(fun i ->
        let high = ((i + 1) * Config.simd_width) - 1 in
        let low = i * Config.simd_width in
        row.:[high, low])
      |> Array.rev
    in
    let hit_nw_windows = windows_of_row i.hit_splitters.shl in
    let hit_no_windows = windows_of_row i.hit_splitters.reg in
    let hit_ne_windows = windows_of_row i.hit_splitters.shr in
    let west_halos =
      Array.init iter_per_lane ~f:(fun _ ->
        Always.Variable.reg spec ~width:Config.simd_cell_width)
    in
    let east_halos =
      Array.init iter_per_lane ~f:(fun _ ->
        Always.Variable.reg spec ~width:Config.simd_cell_width)
    in
    let overflow_reg = Always.Variable.reg spec ~width:1 in
    compile
      [ state.switch
          [ ( Boot
            , [ (* Fetch the initial sources using Stencil boot mode *)
                iter <--. 0
              ; overflow_reg <--. 0
              ; boot_d <--. 1
              ; when_ i.sim_ready [ state.set_next SimdWaitData ]
              ] )
          ; SimdWaitRead, [ read_req_d <--. 1; state.set_next SimdWaitData ]
          ; ( SimdWaitData
            , [ when_
                  (ppb.read_ready &: ppb.write_ready &: i.sim_ready)
                  [ state.set_next SimdExecute ]
              ] )
          ; ( SimdExecute
            , (* TODO: Try to clean SimdExecute up *)
              let hit_nw_window = mux iter.value (Array.to_list hit_nw_windows) in
              let hit_no_window = mux iter.value (Array.to_list hit_no_windows) in
              let hit_ne_window = mux iter.value (Array.to_list hit_ne_windows) in
              let west_halo =
                mux
                  iter.value
                  (Signal.zero Config.simd_cell_width
                   :: List.init (iter_per_lane - 1) ~f:(fun chunk ->
                     west_halos.(chunk).value))
              in
              let east_halo =
                mux
                  iter.value
                  (List.init iter_per_lane ~f:(fun chunk ->
                     if chunk < iter_per_lane - 1
                     then east_halos.(chunk).value
                     else Signal.zero Config.simd_cell_width))
              in
              (List.init Config.simd_width ~f:(fun lane ->
                 let bit_idx = Config.simd_width - 1 - lane in
                 let stencil_lane = simd_ccurr.(lane) in
                 let high = lane_high_bit lane in
                 let low = lane_low_bit lane in
                 let center = ppb.output.:[high, low] in
                 (* StencilSIMD feeding *)
                 [ simd_hit_range.(lane)
                   <-- Signal.concat_msb
                         [ hit_nw_window.:[bit_idx, bit_idx]
                         ; hit_no_window.:[bit_idx, bit_idx]
                         ; hit_ne_window.:[bit_idx, bit_idx]
                         ]
                 ; (stencil_lane.nw
                    <--
                    if lane = 0
                    then west_halo
                    else ppb.output.:[lane_high_bit (lane - 1), lane_low_bit (lane - 1)])
                 ; stencil_lane.no <-- center
                 ; (stencil_lane.ne
                    <--
                    if lane = Config.simd_width - 1
                    then east_halo
                    else ppb.output.:[lane_high_bit (lane + 1), lane_low_bit (lane + 1)])
                 ]
                 (* Save halos *)
                 @ (if lane = Config.simd_width - 1
                    then
                      List.init iter_per_lane ~f:(fun chunk ->
                        when_ (iter.value ==:. chunk) [ west_halos.(chunk) <-- center ])
                    else [])
                 @
                 if lane = 0
                 then
                   List.init (iter_per_lane - 1) ~f:(fun chunk ->
                     when_ (iter.value ==:. chunk + 1) [ east_halos.(chunk) <-- center ])
                 else [])
               |> List.concat)
              @ [ state.set_next SimdWaitSimd ] )
          ; ( SimdWaitSimd
            , [ when_
                  simd.ready
                  [ ppb_write_d <-- (Array.to_list simd.cnext |> Signal.concat_msb)
                  ; write_req_d <--. 1
                  ; when_ (simd.overflow <>:. 0) [ overflow_reg <--. 1 ]
                  ; state.set_next SimdWaitWrite
                  ]
              ] )
          ; ( SimdWaitWrite
            , [ when_
                  ppb.write_ready
                  [ if_
                      (iter.value <:. iter_per_lane - 1)
                      [ write_addr <-- write_addr.value +:. 1
                      ; read_addr <-- read_addr.value +:. 1
                      ; iter <-- iter.value +:. 1
                      ; state.set_next SimdWaitRead
                      ]
                      [ state.set_next SimdFinished ]
                  ]
              ] )
          ; ( SimdFinished
            , [ boot_d <--. 0
              ; iter <--. 0
              ; swap_d <--. 1
              ; write_addr <--. 0
              ; read_addr <--. 0
              ; if_
                  (row_cntr.count ==:. Config.data_depth - 1)
                  [ state.set_next AdderRead ]
                  [ incr_d <--. 1; next_iter_ready_d <--. 1; state.set_next SimdWaitRead ]
              ] )
          ; AdderRead, [ read_req_d <--. 1; state.set_next AdderWaitRead ]
          ; ( AdderWaitRead
            , [ when_ ppb.read_ready [ adder_en_d <--. 1; state.set_next AdderExecute ] ]
            )
          ; ( AdderExecute
            , [ adder_en_d <--. 1
              ; when_
                  adder_simd.ready
                  [ when_ (adder_simd.carry <>:. 0) [ overflow_reg <--. 1 ]
                  ; accu_en_d <--. 1
                  ; state.set_next AdderWaitAccumulator
                  ]
              ] )
          ; ( AdderWaitAccumulator
            , [ accu_en_d <--. 1
              ; when_
                  sol_accu.ready
                  [ if_
                      (iter.value <:. iter_per_lane - 1)
                      [ read_addr <-- read_addr.value +:. 1
                      ; iter <-- iter.value +:. 1
                      ; state.set_next AdderRead
                      ]
                      [ state.set_next Finished ]
                  ]
              ] )
          ; Finished, [ solution_ready_d <--. 1 ]
          ]
      ];
    { next_iter_ready = next_iter_ready_d.value
    ; solution_ready = solution_ready_d.value
    ; overflow = overflow_reg.value
    ; solution = sol_accu.sum
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"manifold_engine" create input
  ;;
end
