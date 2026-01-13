open Base
open Hardcaml

module Make_ROM (Config : sig
    val rom_content : bool list list
    val latency : int option
  end) =
struct
  match Config.rom_content with
  | [] -> failwith "ROM content list has to have at least one word of data"
  | head :: tail ->
    let head_len = List.length head in
    List.iteri tail ~f:(fun i row ->
      let row_len = List.length row in
      if row_len <> head_len
      then (
        let fws =
          Core.sprintf
            "The word at index %d differs in length (%d) compared to the first word's \
             length (%d)."
            (i + 1)
            row_len
            head_len
        in
        failwith fws))
  ;;

  let data_width = List.hd_exn Config.rom_content |> List.length
  let addr_width = List.length Config.rom_content |> Int.ceil_log2

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; read_addr : 'a [@bits addr_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits data_width] } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let rows =
      List.map Config.rom_content ~f:(fun row ->
        List.map row ~f:Signal.of_bool |> Signal.concat_lsb)
    in
    let data =
      let sel_data = Signal.mux i.read_addr rows in
      match Config.latency with
      | None -> sel_data
      | Some latency ->
        let spec = Reg_spec.create ~clock:i.clock ~clear:Signal.gnd () in
        Signal.pipeline spec ~n:latency sel_data
    in
    { data = Signal.mux2 i.enable data (Signal.zero data_width) }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"rom" create input
  ;;
end
