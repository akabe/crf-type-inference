open Format
open Slap.D
open Misc

type ('a, 'b) opt_loop = Continue of 'a | Quit of 'b

exception Divergence

(** {2 Gradient ascent} *)

let grad_ascent ?(eta = 1.0) ?(dec_eta = 1.0) gradient samples x0 f init =
  let rec aux eta x acc =
    let dJ_dx = gradient samples x in
    match f x dJ_dx acc with
    | Quit r -> r
    | Continue acc' ->
      match classify_float (dot dJ_dx dJ_dx) with
      | FP_infinite | FP_nan -> raise Divergence
      | _ ->
        printf "dJ/dw = [@[%a@]]@\n" Slap.Io.pp_rfvec dJ_dx;
        axpy ~alpha:eta dJ_dx x;
        aux (dec_eta *. eta) x acc'
  in
  printf "optimize: GD: eta=%g, dec_eta=%g@." eta dec_eta;
  aux eta x0 init
(*
let sgd
    ~rng ?(batch_size = 100) ?(eta = 1.0) ?(dec_eta = 1.0)
    gradient samples x0 f init =
  let rec aux eta x acc =
    match f x acc with
    | Quit r -> r
    | Continue acc' ->
      let batch = sampling rng batch_size samples in
      printf "optimize: batch=%d subterms@."
        (List.map Lambda.size batch |> List.fold_left (+) 0);
      let dJ_dx = gradient batch x in
      match classify_float (dot dJ_dx) with
      | FP_infinite | FP_nan -> raise Divergence
      | _ ->
        axpy ~alpha:eta dJ_dx x;
        aux (dec_eta *. eta) x acc'
  in
  printf "optimize: SGD: batch_size=%d, eta=%g, dec_eta=%g@."
    batch_size eta dec_eta;
  aux eta x0 init

let sgd_ada_grad
    ~rng ?(batch_size = 100) ?(eta = 1.0) ?(dec_eta = 1.0)
    gradient samples x0 f init =
  let deno = Vec.make1 (Vec.dim x0) in
  let rec aux i eta x acc =
    match f x acc with
    | Quit r -> r
    | Continue acc' ->
      let batch = sampling rng batch_size samples in
      let dJ_dx = gradient batch x in
      match classify_float (dot dJ_dx) with
      | FP_infinite | FP_nan -> raise Divergence
      | _ ->
        if i mod 10 = 0 then Vec.fill deno 1.0;
        ignore (Vec.zpxy ~z:deno dJ_dx dJ_dx);
        axpy ~alpha:eta (Vec.div dJ_dx (Vec.sqrt deno)) x;
        aux (i + 1) (dec_eta *. eta) x acc'
  in
  printf "optimize: SGD+AdaGrad: batch_size=%d, eta=%g, dec_eta=%g@."
    batch_size eta dec_eta;
  aux 1 eta x0 init
*)
