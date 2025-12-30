open Base
open Hardcaml

module Make_PingPongBuffer (Config : sig
    val data_width : int
  end) =
struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; swap : 'a
      ; input : 'a [@bits Config.data_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { output : 'a [@bits Config.data_width] } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let prev = Signal.reg spec ~enable:Signal.vdd i.swap in
    let open Signal in
    let ff_sel =
      Signal.reg_fb spec ~enable:Signal.vdd ~width:1 ~f:(fun sel ->
        (i.swap &: ~:prev) ^: sel)
    in
    let ping_buf = Signal.reg spec ~enable:(~:ff_sel &: i.enable) i.input in
    let pong_buf = Signal.reg spec ~enable:(ff_sel &: i.enable) i.input in
    { O.output = mux2 ff_sel ping_buf pong_buf }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"pingpongbuf" create input
  ;;
end
