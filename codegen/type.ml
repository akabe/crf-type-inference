open Format

type type_var = int

type t =
  | Tbool
  | Tvar of type_var
  | Tarrow of t * t

type constraint_set = (t * t) list

(* type unifier = (type_var * t) list *)

let genvar =
  let c = ref 0 in
  fun () -> incr c ; Tvar !c

(** [subst_type x t u] substitutes all occurrence of type variable [x] in type
    [u] with [t]. *)
let rec subst_type x t = function
  | Tbool -> Tbool
  | Tvar y -> if x = y then t else Tvar y
  | Tarrow (u1, u2) -> Tarrow (subst_type x t u1, subst_type x t u2)

let subst_constraint_set x t cs =
  List.map (fun (u1, u2) -> (subst_type x t u1, subst_type x t u2)) cs

let rec occurs_check x = function
  | Tbool -> true
  | Tvar y -> x <> y
  | Tarrow (t1, t2) -> occurs_check x t1 && occurs_check x t2

(* let rec unify = function *)
(*   | [] -> [] *)
(*   | (t1, t2) :: cs when t1 = t2 -> unify cs *)
(*   | (Tvar x, t) :: cs | (t, Tvar x) :: cs -> *)
(*     if occurs_check x t then (x, t) :: unify (subst_constraint_set x t cs) *)
(*     else failwith "Occurs check failure" *)
(*   | (Tarrow (t1, t2), Tarrow (u1, u2)) :: cs -> *)
(*     unify ((t1, u1) :: (t2, u2) :: cs) *)
(*   | _ -> failwith "Unification failure" *)

let rec unifiable = function
  | [] -> true
  | (t1, t2) :: cs when t1 = t2 -> unifiable cs
  | (Tvar x, t) :: cs | (t, Tvar x) :: cs ->
    occurs_check x t && unifiable (subst_constraint_set x t cs)
  | (Tarrow (t1, t2), Tarrow (u1, u2)) :: cs ->
    unifiable ((t1, u1) :: (t2, u2) :: cs)
  | _ -> false

(* let apply_unifier unifier u0 = *)
(*   List.fold_left (fun u (x, t) -> subst_type x t u) u0 unifier *)

(* let typing ctx e = *)
(*   let open Lambda in *)
(*   let (@) = List.rev_append in *)
(*   let rec aux ctx e = match e.desc with *)
(*     | Etrue -> ([], { desc = Etrue; typ = Tbool; }) *)
(*     | Efalse -> ([], { desc = Efalse; typ = Tbool; }) *)
(*     | Evar x -> ([], { desc = Evar x; typ = List.assoc x ctx; }) *)
(*     | Eif (e1, e2, e3) -> *)
(*       let (c1, e1) = aux ctx e1 in *)
(*       let (c2, e2) = aux ctx e2 in *)
(*       let (c3, e3) = aux ctx e3 in *)
(*       let c = (e1.typ, Tbool) :: (e2.typ, e3.typ) :: c1 @ c2 @ c3 in *)
(*       (c, { desc = Eif (e1, e2, e3); typ = e2.typ; }) *)
(*     | Eapp (e1, e2) -> *)
(*       let (c1, e1) = aux ctx e1 in *)
(*       let (c2, e2) = aux ctx e2 in *)
(*       let typ = genvar () in *)
(*       let c = (e1.typ, Tarrow (e2.typ, typ)) :: c1 @ c2 in *)
(*       (c, { desc = Eapp (e1, e2); typ; }) *)
(*     | Eabs (x, (), e1) -> *)
(*       let t = genvar () in *)
(*       let (c1, e1) = aux ((x, t) :: ctx) e1 in *)
(*       (c1, { desc = Eabs (x, t, e1); typ = Tarrow (t, e1.typ); }) *)
(*   in *)
(*   let (c, e) = aux ctx e in *)
(*   Lambda.map_type (apply_unifier (unify c)) e *)

let pp =
  let rec aux b ppf = function
    | Tbool -> pp_print_string ppf "bool"
    | Tvar x -> fprintf ppf "'a%d" x
    | Tarrow (t1, t2) ->
      let (fmt : _ format) = if b then "(%a -> %a)" else "%a -> %a" in
      fprintf ppf fmt (aux true) t1 (aux false) t2
  in
  aux false

(** {2 Parsing} *)

(* type token = ARR | TYP of t *)

(* let parse s = *)
(*   let rec aux = function *)
(*     | [TYP t] -> t *)
(*     | tokens -> aux (List.rev (reduce [] tokens)) *)
(*   and reduce rev_tkns = function *)
(*     | [] -> failwith "aux error" *)
(*     | [TYP t1; ARR; TYP t2] -> TYP (Tarrow (t1, t2)) :: rev_tkns *)
(*     | hd :: tl -> reduce (hd :: rev_tkns) tl *)
(*   and lex rev_tkns = function *)
(*     | [] -> (List.rev rev_tkns, []) *)
(*     | ')' :: chrs -> (List.rev rev_tkns, chrs) *)
(*     | '(' :: chrs -> *)
(*       let (tkns, rest_chrs) = lex [] chrs in *)
(*       lex (TYP (aux tkns) :: rev_tkns) rest_chrs *)
(*     | ' ' :: chrs -> lex rev_tkns chrs *)
(*     | '-' :: '>' :: chrs -> lex (ARR :: rev_tkns) chrs *)
(*     | 'b' :: 'o' :: 'o' :: 'l' :: chrs -> lex (TYP Tbool :: rev_tkns) chrs *)
(*     | c :: _ -> failwith ("Unknown character: " ^ Char.escaped c) *)
(*   in *)
(*   let explode s = *)
(*     let rec aux i l = if i < 0 then l else aux (i - 1) (s.[i] :: l) in *)
(*     aux (String.length s - 1) [] *)
(*   in *)
(*   explode s |> lex [] |> fst |> aux *)
