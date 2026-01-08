open Base
open Hardcaml

module Make_BRAM (Config : sig
    val data_width : int
    val depth : int
  end) =
struct
  let addr_width = Int.ceil_log2 Config.depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; write_enable : 'a
      ; read_enable : 'a
      ; write_addr : 'a [@bits addr_width]
      ; write_data : 'a [@bits Config.data_width]
      ; read_addr : 'a [@bits addr_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { read_data : 'a [@bits Config.data_width] } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let write_port =
      { Write_port.write_clock = i.clock
      ; write_address = i.write_addr
      ; write_enable = i.write_enable
      ; write_data = i.write_data
      }
    in
    let read_port =
      { Read_port.read_clock = i.clock
      ; read_address = i.read_addr
      ; read_enable = i.read_enable
      }
    in
    let read_data =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:Config.depth
        ~write_ports:[| write_port |]
        ~read_ports:[| read_port |]
        ()
    in
    { O.read_data = read_data.(0) }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"bram" create input
  ;;
end
