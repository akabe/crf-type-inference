open Misc
open Lambda

module Vec = struct
  let make0 n = Array.make n 0.0
  let dim = Array.length
end

let dot x y =
  let acc = ref 0.0 in
  Array.iteri (fun i xi -> acc := !acc +. xi *. y.(i)) x;
  !acc

let axpy ?(alpha = 1.0) x y =
  Array.iteri (fun i xi -> y.(i) <- alpha *. xi +. y.(i)) x

let scal alpha x =
  Array.iteri (fun i xi -> x.(i) <- alpha *. xi) x

let graph_feature ff e =
  let (+) x y = axpy ~alpha:1.0 x y ; y in
  let (++) x = function None -> x | Some y -> x + y in
  let rec aux e = match e.desc with
    | Etrue | Efalse -> Some (ff e)
    | Evar _ -> None
    | Eabs (x, t, e1) -> Some (ff e + ff { desc=Evar x; typ=t; } ++ aux e1)
    | Eapp (e1, e2) -> Some (ff e ++ aux e1 ++ aux e2)
    | Eif (e1, e2, e3) -> Some (ff e ++ aux e1 ++ aux e2 ++ aux e3)
  in
  match aux e with
  | Some x -> x
  | None -> assert false

let graph_log_potential ff w e = dot w (graph_feature ff e)

let graph_potential ff w e = exp (graph_log_potential ff w e)

let vertex_log_potential ff w e = dot w (ff e)

let vertex_potential ff w e = exp (vertex_log_potential ff w e)

(** {2 Normalizer} *)

let normalizer ~types ff w e =
  let pf = vertex_potential ff w in
  let dum = Obj.magic () in
  let rec aux ctx e = match e.desc with
    | Evar x ->
      [({ desc = Evar x; typ = List.assoc x ctx }, 1.0)]
    | Etrue | Efalse as desc ->
      List.map (fun typ -> let e = { desc; typ; } in (e, pf e)) types
    | Eabs (x, _, e2) ->
      let res12 = List.fold_right
          (fun t acc ->
             let p1 = pf { desc = Evar x; typ = t; } in
             List.fold_right
               (fun (te2, p2) acc' -> (t, te2, p1 *. p2) :: acc')
               (aux ((x, t) :: ctx) e2) acc)
          types [] in
      List.map
        (fun typ ->
           let p = csum
               (fun (t, te2, p12) ->
                  let e = { desc = Eabs (x, t, te2); typ; } in
                  pf e *. p12)
               res12 in
           let desc = Eabs (x, dum, dum) in
           ({ desc; typ; }, p))
        types
    | Eapp (e1, e2) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      List.map
        (fun typ ->
           let p = csum2
               (fun (te1, p1) (te2, p2) ->
                  let e = { desc = Eapp (te1, te2); typ; } in
                  pf e *. p1 *. p2)
               res1 res2 in
           let desc = Eapp (dum, dum) in
           ({ desc; typ; }, p))
        types
    | Eif (e1, e2, e3) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      let res3 = aux ctx e3 in
      List.map
        (fun typ ->
           let p = csum3
               (fun (te1, p1) (te2, p2) (te3, p3) ->
                  let e = { desc = Eif (te1, te2, te3); typ; } in
                  pf e *. p1 *. p2 *. p3)
               res1 res2 res3 in
           let desc = Eif (dum, dum, dum) in
           ({ desc; typ; }, p))
        types
  in
  aux [] e
  |> csum snd
  |> ( /. ) 1.0

let log_likelihood ~types ff samples w =
  List.fold_left
    (fun acc e ->
       let z = log (normalizer ~types ff w e) in
       let p = graph_log_potential ff w e +. z in
       acc +. p) 0.0 samples

let log_posterior ~types ~sigma2 ff samples w =
  log_likelihood ~types ff samples w -. dot w w /. (2.0 *. sigma2)

let grad_log_likelihood ~types ff samples w =
  let dum = Obj.magic () in (* a dummy expression *)
  let ( *~ ) a x = scal a x ; x in
  let zero = Vec.make0 (Vec.dim w) in
  let rec aux ctx e = match e.desc with
    | Evar x ->
      [({ desc = Evar x; typ = List.assoc x ctx }, 1.0, zero)]
    | Etrue | Efalse as desc ->
      List.map (fun typ ->
          let e = { desc; typ; } in
          let x = ff e in
          let pot = exp (dot w x) in
          (e, pot, pot *~ x)) types
    | Eabs (x, _, e2) ->
      let res12 = List.fold_right
          (fun t acc ->
             let e1 = { desc = Evar x; typ = t; } in
             let x1 = ff e1 in
             let (mu1, theta1) = (exp (dot w x1), x1) in
             List.fold_right
               (fun (e2, mu2, theta2) acc' ->
                  let mu12 = mu1 *. mu2 in
                  scal mu1 theta2;
                  axpy ~alpha:mu2 theta1 theta2;
                  (t, e2, mu12, theta2) :: acc')
               (aux ((x, t) :: ctx) e2) acc)
          types [] in
      List.map
        (fun typ ->
           let theta0 = Vec.make0 (Vec.dim w) in
           let mu0 = csum
               (fun (t, e2, mu12, theta12) ->
                  let e = { desc = Eabs (x, t, e2); typ; } in
                  let x = ff e in
                  let pot = exp (dot w x) in
                  axpy ~alpha:(pot *. mu12) x theta0;
                  axpy ~alpha:pot theta12 theta0;
                  pot *. mu12)
               res12 in
           let desc = Eabs (x, dum, dum) in
           ({ desc; typ; }, mu0, theta0))
        types
    | Eapp (e1, e2) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      List.map
        (fun typ ->
           let theta0 = Vec.make0 (Vec.dim w) in
           let mu0 = csum2
               (fun (e1, mu1, theta1) (e2, mu2, theta2) ->
                  let e = { desc = Eapp (e1, e2); typ; } in
                  let x = ff e in
                  let pot = exp (dot w x) in
                  let mu0 = pot *. mu1 *. mu2 in
                  axpy ~alpha:mu0 x theta0;
                  axpy ~alpha:(pot *. mu2) theta1 theta0;
                  axpy ~alpha:(pot *. mu1) theta2 theta0;
                  mu0)
               res1 res2 in
           let desc = Eapp (dum, dum) in
           ({ desc; typ; }, mu0, theta0))
        types
    | Eif (e1, e2, e3) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      let res3 = aux ctx e3 in
      List.map
        (fun typ ->
           let theta0 = Vec.make0 (Vec.dim w) in
           let mu0 = csum3
               (fun (e1, mu1, theta1) (e2, mu2, theta2) (e3, mu3, theta3) ->
                  let e = { desc = Eif (e1, e2, e3); typ; } in
                  let x = ff e in
                  let pot = exp (dot w x) in
                  let mu0 = pot *. mu1 *. mu2 *. mu3 in
                  axpy ~alpha:mu0 x theta0;
                  axpy ~alpha:(pot *. mu2 *. mu3) theta1 theta0;
                  axpy ~alpha:(pot *. mu1 *. mu3) theta2 theta0;
                  axpy ~alpha:(pot *. mu1 *. mu2) theta3 theta0;
                  mu0) res1 res2 res3 in
           let desc = Eapp (dum, dum) in
           ({ desc; typ; }, mu0, theta0))
        types
  in
  let dJ_dw = Vec.make0 (Vec.dim w) in
  List.iter
    (fun e ->
       let v = Vec.make0 (Vec.dim w) in
       let z =
         aux [] e
         |> List.fold_left
           (fun acc (_, mu, theta) -> axpy theta v ; acc +. mu)
           0.0 in
       axpy (graph_feature ff e) dJ_dw;
       axpy ~alpha:((-1.0) /. z) v dJ_dw)
    samples;
  dJ_dw

let grad_log_posterior ~types ~sigma2 ff samples w =
  let dJ_dw = grad_log_likelihood ~types ff samples w in
  axpy ~alpha:((-1.0 /. sigma2)) w dJ_dw;
  dJ_dw

(** {2 Inference} *)

let infer ~types lpf e =
  let rec aux ctx e = match e.desc with
    | Evar x ->
      [({ desc = Evar x; typ = List.assoc x ctx }, 0.0)]
    | Etrue | Efalse as desc ->
      List.map (fun typ -> let e = { desc; typ; } in (e, lpf e)) types
    | Eabs (x, _, e2) ->
      let res12 = List.fold_right
          (fun t acc ->
             let lp1 = lpf { desc = Evar x; typ = t; } in
             List.fold_right
               (fun (te2, lp2) acc' -> (t, te2, lp1 +. lp2) :: acc')
               (aux ((x, t) :: ctx) e2) acc)
          types [] in
      List.map
        (fun typ ->
           cmax
             (fun (t, te2, lp12) ->
                let e = { desc = Eabs (x, t, te2); typ; } in
                (e, lpf e +. lp12))
             res12)
        types
    | Eapp (e1, e2) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      List.map
        (fun typ ->
           cmax2
             (fun (te1, lp1) (te2, lp2) ->
                let e = { desc = Eapp (te1, te2); typ; } in
                (e, lpf e +. lp1 +. lp2))
             res1 res2)
        types
    | Eif (e1, e2, e3) ->
      let res1 = aux ctx e1 in
      let res2 = aux ctx e2 in
      let res3 = aux ctx e3 in
      List.map
        (fun typ ->
           cmax3
             (fun (te1, lp1) (te2, lp2) (te3, lp3) ->
                let e = { desc = Eif (te1, te2, te3); typ; } in
                (e, lpf e +. lp1 +. lp2 +. lp3))
             res1 res2 res3)
        types
  in
  aux [] e |> cmax identity

(** {2 Parameter estimation (Gibbs sampling)} *)

(** {2 Parameter estimation (brute force)} *)

module BruteForce = struct
  (** Try all combinations of types for a given expression. *)
  let fold ~types f acc0 e =
    let rec assign ctx e ts = match e.desc, ts with
      | Etrue, typ :: ts -> ({ desc = Etrue; typ; }, ts)
      | Efalse, typ :: ts -> ({ desc = Efalse; typ; }, ts)
      | Evar x, _ -> ({ desc = Evar x; typ = List.assoc x ctx; }, ts)
      | Eabs (x, _, e1), typ :: t :: ts ->
        let (e1, ts) = assign ((x, t) :: ctx) e1 ts in
        ({ desc = Eabs (x, t, e1); typ; }, ts)
      | Eapp (e1, e2), typ :: ts ->
        let (e1, ts) = assign ctx e1 ts in
        let (e2, ts) = assign ctx e2 ts in
        ({ desc = Eapp (e1, e2); typ; }, ts)
      | Eif (e1, e2, e3), typ :: ts ->
        let (e1, ts) = assign ctx e1 ts in
        let (e2, ts) = assign ctx e2 ts in
        let (e3, ts) = assign ctx e3 ts in
        ({ desc = Eif (e1, e2, e3); typ; }, ts)
      | _ -> failwith "Crf.BruteForce.fold"
    in
    size e
    |> init (fun _ -> types)
    |> cfoldN (fun acc ts -> assign [] e ts |> fst |> f acc) acc0

  let grad_log_likelihood ~types ff exprs w =
    let dJ_dw = Vec.make0 (Vec.dim w) in
    let aux e =
      let y = Vec.make0 (Vec.dim w) in
      let z = fold ~types
          (fun acc e' ->
             let x = graph_feature ff e' in
             let p = exp (dot w x) in
             axpy ~alpha:p x y ; acc +. p) 0.0 e in
      axpy (graph_feature ff e) dJ_dw;
      axpy ~alpha:(-1.0 /. z) y dJ_dw
    in
    List.iter aux exprs;
    dJ_dw

  let grad_log_posterior ~types ~sigma2 ff es w =
    let dJ_dw = grad_log_likelihood ~types ff es w in
    axpy ~alpha:((-1.) /. sigma2) w dJ_dw;
    dJ_dw

  let normalizer ~types ff w e =
    1.0 /. fold ~types (fun acc e' -> acc +. graph_potential ff w e') 0.0 e

  let prob ~types ff w e = graph_potential ff w e *. normalizer ~types ff w e

  let infer ~types ff w e =
    let aux opt e =
      let lp = graph_log_potential ff w e in
      match opt with
      | None -> Some (e, lp)
      | Some (e', lp') -> if lp > lp' then Some (e, lp) else Some (e', lp')
    in
    match fold ~types aux None e with
    | None -> failwith "Crf.BruteForce.infer"
    | Some x -> x
end

(** {2 Evaluation} *)
(*
type 'a key = Type of 'a | Total
type 'a confusion_matrix = 'a list * ('a key * 'a key, int) Hashtbl.t

let init_confusion_matrix ~types =
  let tbl = Hashtbl.create 3 in
  Hashtbl.add tbl (Total, Total) 0;
  List.iter
    (fun t ->
       Hashtbl.add tbl (Type t, Total) 0;
       Hashtbl.add tbl (Total, Type t) 0;
       List.iter (fun u -> Hashtbl.add tbl (Type t, Type u) 0) types)
    types;
  tbl

let confusion_matrix ~types ~unifiable eps =
  let cmat = init_confusion_matrix ~types in
  let incr key = Hashtbl.add cmat key (succ (Hashtbl.find cmat key)) in
  let incr_cell t u =
    incr (Type u, Type t);
    incr (Type u, Total);
    incr (Total, Type t);
    incr (Total, Total)
  in
  let bindings = ref [] in
  let aux t1 t2 () = (* t1 = answer (type variables contained),
                        t2 = prediction (with no type variables) *)
    if unifiable bindings t1 t2 then incr_cell t2 t2
    else begin
      let u =
        try List.find (unifiable (ref []) t1) types
        with Not_found -> List.find (( <> ) t2) types in
      incr_cell u t2
    end
  in
  List.iter (fun (e1, e2) -> Lambda.fold2_type aux e1 e2 ()) eps;
  (types, cmat)

let pp_confusion_matrix pp_type ppf (types, cmat) =
  let open Format in
  let types = Array.of_list types in
  let n = Array.length types in
  let pp_label ppf i =
    if i <= n then pp_type ppf types.(i-1)
    else pp_print_string ppf "Total"
  in
  let pp_end_col ppf ~row:_ ~col:_ = pp_print_string ppf " | " in
  let get_el i j =
    let k1 = if i <= n then Type types.(i-1) else Total in
    let k2 = if j <= n then Type types.(j-1) else Total in
    Hashtbl.find cmat (k1, k2)
  in
  Slap.Io.pp_table ~pp_end_col ~pp_left:pp_label ~pp_head:pp_label
    pp_print_int ppf (n + 1) (n + 1) get_el

let accuracy (types, cmat) =
  let nume =
    List.fold_left
      (fun acc t -> acc +. float (Hashtbl.find cmat (Type t, Type t)))
      0.0 types in
  nume /. float (Hashtbl.find cmat (Total, Total))

type score =
  {
    precision : float;
    recall : float;
    f1 : float;
  }

let score_matrix (types, cmat) =
  let (//) x y = float x /. float y in
  let aux t =
    let key = Type t in
    let deno_p = Hashtbl.find cmat (Total, key) in
    let deno_r = Hashtbl.find cmat (key, Total) in
    let nume = Hashtbl.find cmat (key, key) in
    if deno_p = 0 || deno_r = 0 || nume = 0 then None
    else begin
      let precision = nume // deno_p in
      let recall = nume // deno_r in
      let f1 = 2.0 *. precision *. recall /. (precision +. recall) in
      Some (t, { precision; recall; f1; })
    end
  in
  List.map aux types

let pp_score_matrix pp_type ppf smat =
  let open Format in
  let smat = filter_map identity smat in
  let n = List.length smat in
  let pp_left ppf i = pp_type ppf (fst (List.nth smat (i - 1))) in
  let pp_head ppf i =
    pp_print_string ppf [|"Precision"; "Recall"; "F1"|].(i-1)
  in
  let pp_end_col ppf ~row:_ ~col:_ = pp_print_string ppf " | " in
  let get_el i j =
    let (_, s) = List.nth smat (i - 1) in
    if j = 1 then s.precision else if j = 2 then s.recall else s.f1
  in
  Slap.Io.pp_table ~pp_end_col ~pp_left ~pp_head
    (fun ppf -> fprintf ppf "%g") ppf n 3 get_el
*)
