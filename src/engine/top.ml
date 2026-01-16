open Base
open Hardcaml

module Make_Top (Config : sig
    val data_width : int
    val data_depth : int
    val max_num1 : int
    val max_num2 : int
    val mem_fetch_delay : int
    val mem_write_delay : int
    val simd_width : int
    val simd_cell_width : int
    val rom_content : bool list list option
  end) =
struct
  let addr_width = Int.ceil_log2 Config.data_depth
  let sol1_width = Int.ceil_log2 Config.max_num1
  let sol2_width = Int.ceil_log2 Config.max_num2
  let is_ext_mem = Option.is_none Config.rom_content

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; data : 'a option [@exists is_ext_mem] [@bits Config.data_width]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { solution1 : 'a [@bits sol1_width]
      ; solution2 : 'a [@bits sol2_width]
      ; solutions_ready : 'a
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module SLatch =
      Slatch.Make_SLatch (struct
        let data_width = Config.data_width
        let data_depth = Config.data_depth
        let mem_fetch_delay = Config.mem_fetch_delay
      end)
    in
    let module BeamEngine =
      Beam_engine.Make_BeamEngine (struct
        let data_width = Config.data_width
        let data_depth = Config.data_depth
        let max_value = Config.max_num1
      end)
    in
    let module ManifoldEngine =
      Manifold_engine.Make_ManifoldEngine (struct
        let data_width = Config.data_width
        let data_depth = Config.data_depth
        let simd_cell_width = Config.simd_cell_width
        let simd_width = Config.simd_width
        let max_value = Config.max_num2
        let mem_fetch_delay = Config.mem_fetch_delay
        let mem_write_delay = Config.mem_write_delay
      end)
    in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
    let open Signal in
    let ready_reset_d = Signal.wire 1 in
    let%hw first_kick =
      Signal.reg_fb spec ~enable:Signal.vdd ~width:2 ~f:(fun is_kicked ->
        Signal.mux2
          (is_kicked ==:. 0)
          (Signal.of_int ~width:2 1)
          (Signal.of_int ~width:2 2))
    in
    let%hw beams_rdy_d = Signal.wire 1 in
    let%hw beams_next_rdy =
      Signal.reg_fb spec ~enable:Signal.vdd ~width:1 ~f:(fun e ->
        e |: beams_rdy_d &: ~:ready_reset_d)
    in
    let%hw manifold_rdy_d = Signal.wire 1 in
    let%hw manifold_next_rdy =
      Signal.reg_fb spec ~enable:Signal.vdd ~width:1 ~f:(fun e ->
        e |: manifold_rdy_d |: first_kick.:[0, 0] &: ~:ready_reset_d)
    in
    let%hw slatch_next_d = beams_next_rdy &: manifold_next_rdy in
    let slatch_addr_d = Signal.wire addr_width in
    let () = ready_reset_d <== slatch_next_d in
    let slatch =
      SLatch.hierarchical
        scope
        { clock = i.clock
        ; reset = i.reset
        ; step = slatch_next_d
        ; data_in =
            (if is_ext_mem
             then Option.value ~default:(Signal.zero Config.data_width) i.data
             else
               let module ROM =
                 Common.Rom.Make_ROM (struct
                   let rom_content = Option.value ~default:[] Config.rom_content
                   let latency = None
                 end)
               in
               let rom =
                 ROM.hierarchical
                   scope
                   { clock = i.clock; enable = Signal.vdd; read_addr = slatch_addr_d }
               in
               rom.data)
        }
    in
    let () = slatch_addr_d <== slatch.addr in
    let beam_engine =
      BeamEngine.hierarchical
        scope
        { clock = i.clock
        ; reset = i.reset
        ; mem_ready = slatch.ready
        ; mem_data = slatch.data_out
        }
    in
    let () = beams_rdy_d <== beam_engine.next_iter_ready in
    let manifold_engine =
      ManifoldEngine.hierarchical
        scope
        { clock = i.clock
        ; reset = i.reset
        ; enable = Signal.vdd
        ; sim_ready = beam_engine.hit_splitters_ready
        ; hit_splitters =
            (let hit = beam_engine.hit_splitters in
             let x =
               { ManifoldEngine.HitSplitterLane.shl = sll hit 1
               ; cen = hit
               ; shr = srl hit 1
               }
             in
             x)
        }
    in
    let () = manifold_rdy_d <== manifold_engine.next_iter_ready in
    let error_reg =
      Signal.reg_fb spec ~width:1 ~f:(fun e -> e |: manifold_engine.overflow)
    in
    { solution1 = beam_engine.solution
    ; solution2 = manifold_engine.solution
    ; solutions_ready = beam_engine.solution_ready &: manifold_engine.solution_ready
    ; error = error_reg
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"toplevel" create input
  ;;
end
