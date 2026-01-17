open Core
open Hardcaml
open Solution.Engine.Top

module type Top_module = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module RtlLanguage = struct
  type t = Rtl.Language.t

  let all = [ Rtl.Language.Verilog; Vhdl ]

  let to_string = function
    | Rtl.Language.Verilog -> "verilog"
    | Vhdl -> "vhdl"
  ;;

  let all_fmt = all |> List.map ~f:to_string |> String.concat ~sep:"\n"

  let of_string name =
    match String.lowercase name with
    | "verilog" -> Rtl.Language.Verilog
    | "vhdl" -> Vhdl
    | _ ->
      eprintf
        "Unknown RTL language: \"%s\"!\n\nAvailable RTL languages:\n%s\n\n"
        name
        all_fmt;
      exit 1
  ;;
end

let gen_rtl lang flatten (module Top : Top_module) =
  let module Circuit = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~flatten_design:flatten () in
  let circuit = Circuit.create_exn ~name:"top" (Top.create scope) in
  let database = Scope.circuit_database scope in
  Rtl.output
    ~output_mode:
      (To_file
         (Core.sprintf
            "top.%s"
            (match lang with
             | Rtl.Language.Verilog -> "v"
             | Vhdl -> "vhd")))
    ~database
    lang
    circuit
;;

let boolrows_of_string string_repr =
  List.map string_repr ~f:(fun row ->
    row
    |> String.to_list
    |> List.map ~f:(function
      | 'S' | '^' -> true
      | _ -> false))
;;

let gen_top ~lang ~input_file ~is_rom ~simd_width ~preprocess =
  (* Standard example from Advent of Code Day 7 *)
  let default_input =
    [ ".......S........"
    ; "................"
    ; ".......^........"
    ; "................"
    ; "......^.^......."
    ; "................"
    ; ".....^.^.^......"
    ; "................"
    ; "....^.^...^....."
    ; "................"
    ; "...^.^...^.^...."
    ; "................"
    ; "..^...^.....^..."
    ; "................"
    ; ".^.^.^.^.^...^.."
    ; "................"
    ]
  in
  let data_width, data_depth, rom_content =
    if is_rom
    then (
      match input_file with
      | Some path ->
        let grid = Utils.load_grid ~preprocess path in
        grid.width, grid.height, Some (boolrows_of_string (grid.rows |> Array.to_list))
      | None -> 16, 16, Some (boolrows_of_string default_input))
    else 16, 16, None
  in
  let () =
    Core.printf
      "Generating Top level with SIMD width = %d with data_width = %d; data_depth = %d...\n"
      simd_width
      data_width
      data_depth
  in
  (* TODO: Add more input arguments for all [Make_Top] config fields *)
  let module Top =
    Make_Top (struct
      let data_width = data_width
      let data_depth = data_depth
      let max_num1 = Int.pow 2 16 - 1
      let max_num2 = Int.pow 2 50 - 1
      let mem_fetch_delay = 0
      let mem_write_delay = 0
      let simd_width = simd_width
      let simd_cell_width = 44
      let rom_content = rom_content
    end)
  in
  gen_rtl lang false (module Top : Top_module)
;;

let rtl_lang_arg : RtlLanguage.t Command.Arg_type.t =
  Command.Arg_type.create RtlLanguage.of_string
;;

let command =
  Command.basic
    ~summary:"Generate RTL code"
    ~readme:(fun () -> sprintf "Available RTL languages:\n\n%s\n\n" RtlLanguage.all_fmt)
    (let open Command.Let_syntax in
     let%map_open lang =
       flag
         "lang"
         (optional_with_default Rtl.Language.Verilog rtl_lang_arg)
         ~doc:"LANG RTL language to generate (verilog|vhdl)"
     and input_file =
       flag
         "input"
         (optional string)
         ~doc:"PATH Input file describing the grid (AoC Day 7 format)"
     and ram =
       flag
         "ram"
         no_arg
         ~doc:"Use writable RAM instead of burned-in ROM (ignores [input] argument)"
     and simd_width =
       flag
         "simd-width"
         (optional_with_default 8 int)
         ~doc:"INT The width of the Manifold Engine's SIMD"
     and preprocess =
       flag "preprocess" no_arg ~doc:"Runs the input file through the preprocessor"
     in
     fun () -> gen_top ~lang ~is_rom:(not ram) ~input_file ~simd_width ~preprocess)
;;

let () = Command_unix.run ~version:"1.0.0" command
