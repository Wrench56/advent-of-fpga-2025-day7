open Core

module SimulConfig = struct
  type t =
    { wave_width : int
    ; signals_width : int
    ; values_width : int
    }
end

module Testbench = struct
  type t =
    { name : string
    ; description : string
    ; testbench : unit -> Hardcaml_waveterm.Waveform.t
    ; config : SimulConfig.t option
    }

  let all =
    [ { name = "beam_engine"
      ; description = "Beam Engine (AoC 2025 Day 7 Part 1) Simulation"
      ; testbench = Test.Engine.Test_beam_engine.testbench
      ; config = None
      }
    ; { name = "manifold_engine"
      ; description = "Manifold Engine (AoC 2025 Day 7 Part 2) Simulation"
      ; testbench = Test.Engine.Test_manifold_engine.testbench
      ; config = Some { wave_width = 2; signals_width = 30; values_width = 34 }
      }
    ; { name = "top"
      ; description = "Toplevel (AoC 2025 Day 7 Part 1) Simulation"
      ; testbench = Test.Engine.Test_top.testbench 
      ; config = Some { wave_width = 2; signals_width = 30; values_width = 34 }

      }
    ]
  ;;

  let all_fmt =
    all
    |> List.map ~f:(fun tb -> sprintf " - %-27s %s" tb.name tb.description)
    |> String.concat ~sep:"\n"
  ;;

  let find name =
    match List.find all ~f:(fun t -> String.(t.name = name)) with
    | Some t -> t
    | None ->
      eprintf "Unknown testbench: \"%s\"!\n\nAvailable testbenches:\n%s\n\n" name all_fmt;
      exit 1
  ;;
end

let run_interactive (testbench : Testbench.t) =
  let config =
    match testbench.config with
    | Some conf -> conf
    | None -> { wave_width = 2; signals_width = 20; values_width = 16 }
  in
  let waves = testbench.testbench () in
  Hardcaml_waveterm_interactive.run
    ~wave_width:config.wave_width
    ~signals_width:config.signals_width
    ~values_width:config.values_width
    waves
;;

let testbench_arg : Testbench.t Command.Arg_type.t =
  Command.Arg_type.create Testbench.find
;;

let command =
  Command.basic
    ~summary:"Run interactive waveform viewer (IAV)"
    ~readme:(fun () -> sprintf "Available testbenches:\n\n%s\n\n" Testbench.all_fmt)
    (Command.Param.(map (anon ("testbench" %: testbench_arg))) ~f:(fun testbench () ->
       run_interactive testbench))
;;

let () = Command_unix.run ~version:"1.0.0" command
