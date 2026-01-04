open Base
open Hardcaml

module Make_Adder (Config : sig
    val add_width : int
  end) =
struct
  module I = struct
    type 'a t =
      { num1 : 'a [@bits Config.add_width]
      ; num2 : 'a [@bits Config.add_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { sum : 'a [@bits Config.add_width]
      ; carry : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) : Signal.t O.t =
    let open Signal in
    let cwidth = Config.add_width + 1 in
    let carry_and_sum = uresize i.num1 cwidth +: uresize i.num2 cwidth in
    { sum = lsbs carry_and_sum; carry = msb carry_and_sum }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"adder" create input
  ;;
end
