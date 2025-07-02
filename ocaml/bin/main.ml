open Lib_parser

(*let calc s = print_endline (Float.to_string (fst (Option.get (calculate s))))*)
let calc s = s |> Calculator.calculate |> Option.get |> fst |> Float.to_string |> print_endline

let () =
    calc "72 - 7 - (1 - 2) * 3";                   (* 68. *)
    calc "-7";                                     (* -7. *)
    calc "-2^2";                                   (* -4. *)
    calc " 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)";  (* -8.889 *)
    calc "3^(1+1)^3";                              (* 6561. *)
    calc "sin(1+1)";                               (* 0.909297426826 *)
    calc "sin ( 2_SQRTPI * sqr ( 2 ) - 1 )";       (* -0.363408573143 *)
    calc "sqr(2 + 3)";                             (* 25. *)
    calc "sin(-PI/4)";                             (* -0.707106781187 *)
    calc " E ^ PI";                                (* 23.1406926328 *)
    calc " PI ^ E";                                (* 22.4591577184 *)
