open Format
open Misc
open Utils
open Slap.D
open Lambda
open SimpleType
open Model.M

let rng =
  let rng = Gsl.Rng.make Gsl.Rng.MT19937 in
  Gsl.Rng.set rng (Nativeint.of_int 100);
  rng

let () =
  let sigma2 = 0.1 in
  if Array.length Sys.argv < 3
  then eprintf "Usage: %s DATA_DIR LOG_DIR@." Sys.argv.(0)
  else ["depth1.lam"; "depth2.lam"; "depth3.lam";
        "depth4.lam"; "depth5.lam"; "depth6.lam"]
       |> List.map (fun s -> Sys.argv.(1) ^ "/" ^ s)
       |> load_lambdas
       |> List.map (typing [])
       |> List.map (Lambda.map_type simplify)
       |> List.filter (acceptable ~types ~unifiable)
       |> sampling rng 1000
       |> main
         ~dim ~types ~unifiable
         ~dname:Sys.argv.(2)
         ~pp_type:SimpleType.pp
         ~cv:5
         ~max_loops:100
         (Optimize.grad_ascent ~eta:0.0001 ~dec_eta:1.0
            (Crf.grad_log_posterior ~sigma2 ~types feature))
         (Crf.log_posterior ~sigma2 ~types feature)
         feature
