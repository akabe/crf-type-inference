(** Code generation *)

open Format
open Misc
(** {2 Categorization of generated expressions} *)

type category =
  | Ctrue
  | Cfalse
  | Cabs

let string_of_category = function
  | Ctrue -> "true"
  | Cfalse -> "false"
  | Cabs -> "abs"

let categorize =
  let open Lambda in
  let aux e = match e.desc with
    | Etrue -> Ctrue
    | Efalse -> Cfalse
    | Eabs _ -> Cabs
    | _ -> failwith "unknown eval result"
  in
  let rec loop i e =
    if i = 0 then failwith "unterminated eval"
    else match eval1 e with
      | e' -> loop (i - 1) e'
      | exception NoRuleApplies -> aux e
  in
  loop

(** {2 Code generation} *)

let (@) = List.rev_append

let rng = Gsl.Rng.make Gsl.Rng.MT19937

let prob p = p > Gsl.Rng.uniform rng (* [true] with probability [p] *)

let combinate f xs init = List.fold_left (fun acc x -> f x :: acc) init xs
let combinate2 f xs ys init =
  List.fold_left (fun acc x -> combinate (f x) ys acc) init xs
let combinate3 f xs ys zs init =
  List.fold_left (fun acc x -> combinate2 (f x) ys zs acc) init xs

let gen_abs_append gen_expr t ctx depth acc =
  if depth < 2 then acc
  else begin
    let x = "x" ^ string_of_int (List.length ctx) in
    let t1, t2 = Type.genvar (), Type.genvar () in
    let el2 = gen_expr t2 ((x, t1) :: ctx) (depth - 1) in
    let eqn = (t, Type.Tarrow (t1, t2)) in
    let aux (c, e2) = (eqn :: c,
                       Lambda.({ typ = t; desc = Eabs (x, t1, e2); })) in
    combinate aux el2 acc
  end

let gen_app_append gen_expr t11 ctx depth acc =
  if depth < 2 then acc
  else begin
    let d' = depth - 1 in
    let t12 = Type.genvar () in
    let el1 = gen_expr (Type.Tarrow (t12, t11)) ctx d' in
    let el2 = gen_expr t12 ctx d' in
    let aux (c1, e1) (c2, e2) =
      (c1 @ c2, Lambda.({ typ = t11; desc = Eapp (e1, e2); }))
    in
    combinate2 aux el1 el2 acc
  end

let gen_if_append gen_expr t ctx depth acc =
  if depth < 2 then acc
  else begin
    let d' = depth - 1 in
    let el1 = gen_expr Type.Tbool ctx d' in
    let el2 = gen_expr t ctx d' in
    let el3 = gen_expr t ctx d' in
    let aux (c1, e1) (c2, e2) (c3, e3) =
      (c1 @ c2 @ c3, Lambda.({ typ = t; desc = Eif (e1, e2, e3); }))
    in
    combinate3 aux el1 el2 el3 acc
  end

type setting =
  {
    n_sub : int;
    n_abs : int;
    n_app : int;
    n_if : int;
  }

let gen_expr stg t depth =
  let rec aux n t ctx depth =
    let e_vars =
      List.rev_map
        (fun (x, u) -> ([t, u], Lambda.{ typ = u; desc = Evar x; }))
        ctx in
    let es0 =
      if depth = 1
      then ([t, Type.Tbool], Lambda.{ desc = Etrue; typ = Type.Tbool; })
           :: ([t, Type.Tbool], Lambda.{ desc = Efalse; typ = Type.Tbool; })
           :: e_vars
      else aux stg.n_sub t ctx (depth - 1) in
    es0
    |> gen_abs_append (aux stg.n_abs) t ctx depth
    |> gen_app_append (aux stg.n_app) t ctx depth
    |> gen_if_append (aux stg.n_if) t ctx depth
    |> List.filter (fun (c, _) -> Type.unifiable c)
    |> sampling rng n
  in
  aux max_int t [] depth
  |> List.rev_map (fun (_, e) -> Lambda.map_type ignore e)
  |> List.filter (fun e -> Lambda.depth e = depth)

let generate ?(limit = 1000) nums stg t depth =
  let tbl = Hashtbl.create 5 in
  let check_nums () =
    List.for_all
      (fun (cat, n) -> List.length !(Hashtbl.find tbl cat) >= n)
      nums
  in
  let rec loop () =
    (* Generate expressions. *)
    List.iter
      (fun e ->
         let set = Hashtbl.find tbl (categorize limit e) in
         if not (List.mem e !set) then set := e :: !set)
      (gen_expr stg t depth);
    (* Sampling if a set is too large. *)
    printf "depth=%d:" depth;
    List.iter
      (fun (cat, n) ->
         let set = Hashtbl.find tbl cat in
         if n > 0 then set := sampling rng n !set;
         printf " %s=%d/%d" (string_of_category cat) (List.length !set) n)
      nums;
    print_newline ();
    if check_nums () then tbl else loop ()
  in
  List.iter (fun (cat, _) -> Hashtbl.add tbl cat (ref [])) nums;
  loop ()

(** Code generation for small expressions: if [depth < 4], we can generate all
    expressions becuase the number of them is not large. *)
let generate_small_exprs depth =
    let nums = [ (Ctrue, 0); (Cfalse, 0); (Cabs, 0); ] in
    let stg = { n_sub = max_int;
                n_abs = max_int;
                n_app = max_int;
                n_if = max_int; } in
  generate nums stg (Type.genvar ()) depth

let generate_large_exprs depth =
  let nums = [ (Ctrue, 500); (Cfalse, 500); (Cabs, 500); ] in
  let stg = { n_sub = 1000;
              n_abs = 1000;
              n_app = 100;
              n_if = 10; } in
  generate nums stg (Type.genvar ()) depth

let main dir =
  for depth = 1 to 6 do
    let f = if depth < 4 then generate_small_exprs else generate_large_exprs in
    let tbl = f depth in
    let fname = dir ^ "/depth" ^ string_of_int depth ^ ".lam" in
    let oc = open_out fname in
    let ppf = formatter_of_out_channel oc in
    Hashtbl.iter
      (fun cat set ->
         fprintf ppf "(* %s *)@\n@\n" (string_of_category cat);
         List.iter
           (fprintf ppf "%a ;;@\n@\n" (Lambda.pp_lambda pp_print_string)) !set)
      tbl;
    pp_print_flush ppf ();
    close_out oc
  done

let () =
  if Array.length Sys.argv < 2
  then printf "Usage: %s DIRECTORY@." Sys.argv.(0)
  else main Sys.argv.(1)
