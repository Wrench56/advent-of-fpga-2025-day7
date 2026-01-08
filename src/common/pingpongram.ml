open Base
open Hardcaml

module Make_PingPongRAM (Config : sig
    val data_width : int
    val data_depth : int
    val mem_fetch_delay : int
    val mem_write_delay : int
  end) =
struct
  let addr_width = Config.data_depth * 2 |> Int.ceil_log2

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; write_req : 'a
      ; read_req : 'a
      ; swap : 'a
      ; read_addr : 'a [@bits addr_width]
      ; write_addr : 'a [@bits addr_width]
      ; input : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { output : 'a [@bits Config.data_width]
      ; write_ready : 'a
      ; read_ready : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let module BRAM =
      Bram.Make_BRAM (struct
        let data_width = Config.data_width
        let depth = Config.data_depth * 2
      end)
    in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let prev = Signal.reg spec ~enable:Signal.vdd i.swap in
    let open Signal in
    (*
    Ready logic. Read the docs in `slatch.ml` for more info, its the same system
    *)
    let write_busy_d = wire 1 in
    let write_busy = reg spec ~enable:Signal.vdd write_busy_d in
    let write_accept = i.write_req &: ~:write_busy in
    let write_done_pulse =
      pipeline spec ~enable:Signal.vdd ~n:Config.mem_write_delay write_accept
    in
    write_busy_d <== (write_busy |: write_accept &: ~:write_done_pulse);
    let write_ready = ~:write_busy in
    let read_busy_d = wire 1 in
    let read_busy = reg spec ~enable:Signal.vdd read_busy_d in
    let read_accept = i.read_req &: ~:read_busy in
    let read_done_pulse =
      pipeline spec ~enable:Signal.vdd ~n:Config.mem_fetch_delay read_accept
    in
    read_busy_d <== (read_busy |: read_accept &: ~:read_done_pulse);
    let read_ready = ~:read_busy in
    let rw_ready = read_ready &: write_ready in
    let ff_sel =
      Signal.reg_fb spec ~enable:Signal.vdd ~width:1 ~f:(fun sel ->
        let toggle = i.swap &: ~:prev &: rw_ready in
        mux2 toggle ~:sel sel)
    in
    let half_offset = of_int ~width:addr_width Config.data_depth in
    let backing_mem =
      BRAM.hierarchical
        scope
        { clock = i.clock
        ; write_enable = write_accept
        ; read_enable = read_accept
        ; write_addr =
            i.write_addr +: mux2 (ff_sel &: rw_ready) half_offset (zero addr_width)
        ; read_addr =
            i.read_addr +: mux2 (ff_sel &: rw_ready) (zero addr_width) half_offset
        ; write_data = i.input
        }
    in
    { output = backing_mem.read_data; read_ready; write_ready }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pingpongram" create input
  ;;
end
