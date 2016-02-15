(** Expression of lambda + Bool *)

open Format

type ('a, 'b) t = { typ : 'b; desc : ('a, 'b) desc; }
and ('a, 'b) desc =
  | Evar of 'a
  | Eabs of 'a * 'b * ('a, 'b) t
  | Eapp of ('a, 'b) t * ('a, 'b) t
  | Etrue
  | Efalse
  | Eif of ('a, 'b) t * ('a, 'b) t * ('a, 'b) t
  [@@deriving show]

let tru = { desc = Etrue; typ = (); }
let fls = { desc = Efalse; typ = (); }
let var x = { desc = Evar x; typ = (); }
let lam x e1 = { desc = Eabs (x, (), e1); typ = (); }
let ($) e1 e2 = { desc = Eapp (e1, e2); typ = (); }
let if_ e1 e2 e3 = { desc = Eif (e1, e2, e3); typ = (); }

let isval e = match e.desc with
  | Etrue | Efalse | Eabs _ -> true
  | Evar _ | Eapp _ | Eif _ -> false

let alpha e =
  let genvar =
    let c = ref (-1) in
    fun x -> incr c ; (x, !c)
  in
  let rec aux ctx e =
    let desc = match e.desc with
      | Etrue | Efalse as d -> d
      | Evar x -> Evar (List.assoc x ctx)
      | Eabs (x, t, e1) ->
        let y = genvar x in
        Eabs (y, t, aux ((x, y) :: ctx) e1)
      | Eapp (e1, e2) -> Eapp (aux ctx e1, aux ctx e2)
      | Eif (e1, e2, e3) -> Eif (aux ctx e1, aux ctx e2, aux ctx e3) in
    { e with desc }
  in
  aux [] e

let rec size e = match e.desc with
  | Etrue | Efalse -> 1
  | Evar _ -> 0 (* VAR is counted at ABS *)
  | Eabs (_, _, e1) -> size e1 + 2 (* a formal argument is a node (sharing) *)
  | Eapp (e1, e2) -> size e1 + size e2 + 1
  | Eif (e1, e2, e3) -> size e1 + size e2 + size e3 + 1

let rec depth e = match e.desc with
  | Etrue | Efalse | Evar _ -> 1
  | Eabs (_, _, e1) -> depth e1 + 1
  | Eapp (e1, e2) -> max (depth e1) (depth e2) + 1
  | Eif (e1, e2, e3) -> max (depth e1) (depth e2) |> max (depth e3) |> (+) 1

(** [subst x y e] substitutes variable [x] in expression [e] with [y]. *)
let subst x y =
  let rec aux e = match e.desc with
    | Etrue | Efalse -> e
    | Evar z -> if x = z then y else e
    | Eabs (z, t, e1) ->
      if x = z then e else { e with desc = Eabs (z, t, aux e1) }
    | Eapp (e1, e2) -> { e with desc = Eapp (aux e1, aux e2) }
    | Eif (e1, e2, e3) -> { e with desc = Eif (aux e1, aux e2, aux e3) }
  in
  aux

let rec map_variable f e =
  let desc = match e.desc with
    | Etrue -> Etrue
    | Efalse -> Efalse
    | Evar x -> Evar (f x)
    | Eabs (x, t, e1) -> Eabs (f x, t, map_variable f e1)
    | Eapp (e1, e2) -> Eapp (map_variable f e1, map_variable f e2)
    | Eif (e1, e2, e3) ->
      Eif (map_variable f e1, map_variable f e2, map_variable f e3) in
  { e with desc }

let rec map_type f e =
  let typ = f e.typ in
  let desc = match e.desc with
    | Etrue -> Etrue
    | Efalse -> Efalse
    | Evar x -> Evar x
    | Eabs (x, t, e1) -> Eabs (x, f t, map_type f e1)
    | Eapp (e1, e2) -> Eapp (map_type f e1, map_type f e2)
    | Eif (e1, e2, e3) -> Eif (map_type f e1, map_type f e2, map_type f e3) in
  { typ; desc; }

let rec map2_type f e1 e2 =
  let typ = f e1.typ e2.typ in
  let desc = match e1.desc, e2.desc with
    | Etrue, Etrue -> Etrue
    | Efalse, Efalse -> Efalse
    | Evar x, Evar y when x = y -> Evar x
    | Eabs (x, t, e11), Eabs (y, u, e21) when x = y ->
       Eabs (x, f t u, map2_type f e11 e21)
    | Eapp (e11, e12), Eapp (e21, e22) ->
       Eapp (map2_type f e11 e21, map2_type f e12 e22)
    | Eif (e11, e12, e13), Eif (e21, e22, e23) ->
       Eif (map2_type f e11 e21, map2_type f e12 e22, map2_type f e13 e23)
    | _ -> failwith "Lambda.map2_type" in
  { typ; desc; }

let rec for_all_type f e =
  if f e.typ
  then match e.desc with
    | Etrue | Efalse | Evar _ -> true
    | Eabs (_, t, e1) -> f t && for_all_type f e1
    | Eapp (e1, e2) -> for_all_type f e1 && for_all_type f e2
    | Eif (e1, e2, e3) ->
      for_all_type f e1 && for_all_type f e2 && for_all_type f e3
  else false

let rec fold_type f e acc = match e.desc with
  | Etrue | Efalse -> f e.typ acc
  | Evar _ -> acc
  | Eabs (_, t, e1) -> f e.typ acc
                       |> f t
                       |> fold_type f e1
  | Eapp (e1, e2) -> f e.typ acc
                     |> fold_type f e1
                     |> fold_type f e2
  | Eif (e1, e2, e3) -> f e.typ acc
                        |> fold_type f e1
                        |> fold_type f e2
                        |> fold_type f e3

let rec fold2_type f e1 e2 acc = match e1.desc, e2.desc with
  | Etrue, Etrue | Efalse, Efalse -> f e1.typ e2.typ acc
  | Evar x, Evar y when x = y -> acc
  | Eabs (x, t, e11), Eabs (y, u, e21) when x = y -> f e1.typ e2.typ acc
                                                     |> f t u
                                                     |> fold2_type f e11 e21
  | Eapp (e11, e12), Eapp (e21, e22) -> f e1.typ e2.typ acc
                                        |> fold2_type f e11 e21
                                        |> fold2_type f e12 e22
  | Eif (e11, e12, e13), Eif (e21, e22, e23) -> f e1.typ e2.typ acc
                                                |> fold2_type f e11 e21
                                                |> fold2_type f e12 e22
                                                |> fold2_type f e13 e23
  | _ -> failwith "Lambda.fold2_type"

let untyping e = map_type ignore e

(** {2 Pretty printing} *)

let pp_lambda pp_var ppf e =
  let rec aux p ppf e = match e.desc with
    | Etrue -> pp_print_string ppf "true"
    | Efalse -> pp_print_string ppf "false"
    | Evar s -> pp_var ppf s
    | Eapp (e1, e2) ->
      let (fmt : _ format) =
        if p < 3 then "@[%a@;<1 2>%a@]" else "(@[%a@;<1 2>%a@])" in
      fprintf ppf fmt (aux 2) e1 (aux 3) e2
    | Eif (e1, e2, e3) ->
      let (fmt : _ format) =
        if p < 2 then "@[if@ %a@ then@ %a@ else@ %a@]"
        else "(@[if@ %a@ then@ %a@ else@ %a@])" in
      fprintf ppf fmt (aux 2) e1 (aux 2) e2 (aux 1) e3
    | Eabs (x, _, e) ->
      let (fmt : _ format) =
        if p < 1 then "@[\\%a.@;<1 2>%a@]"
        else "(@[\\%a.@;<1 2>%a@])" in
      fprintf ppf fmt pp_var x (aux 0) e
  in
  aux 0 ppf e

let pp_dot pp_vertex ppf e =
  let genid =
    let c = ref (-1) in
    fun () -> incr c ; !c
  in
  let rec aux ctx e = match e.desc with
    | Evar x -> List.assoc x ctx
    | Etrue | Efalse ->
      let id = genid () in
      fprintf ppf "v%d [%a]@\n" id pp_vertex e;
      id
    | Eabs (x, t, e2) ->
      let id = genid () in
      let id1 = genid () in
      let e1 = { desc = Evar x; typ = t; } in
      let id2 = aux ((x, id1) :: ctx) e2 in
      fprintf ppf "v%d [%a]@\n\
                   v%d [%a]@\n\
                   v%d -> v%d [label=\"1\"]@\n\
                   v%d -> v%d [label=\"2\"]@\n"
        id1 pp_vertex e1 id pp_vertex e id id1 id id2;
      id
    | Eapp (e1, e2) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      fprintf ppf "v%d [%a]@\n\
                   v%d -> v%d [label=\"1\"]@\n\
                   v%d -> v%d [label=\"2\"]@\n"
        id pp_vertex e id id1 id id2;
      id
    | Eif (e1, e2, e3) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      let id3 = aux ctx e3 in
      fprintf ppf "v%d [%a]@\n\
                   v%d -> v%d [label=\"1\"]@\n\
                   v%d -> v%d [label=\"2\"]@\n\
                   v%d -> v%d [label=\"3\"]@\n"
        id pp_vertex e id id1 id id2 id id3;
      id
  in
  pp_print_string ppf "digraph lambda {\n";
  ignore (aux [] e);
  pp_print_string ppf "}\n"

(** {2 Evaluation} *)

exception NoRuleApplies

(** [eval1 e] one-step evaluates expression [e].
    @raise NoRuleApplies if a given expression cannot be reduced anymore. *)
let rec eval1 e = match e.desc with
  | Eif ({ desc = Etrue; _ }, e2, _) -> e2
  | Eif ({ desc = Efalse; _ }, _, e3) -> e3
  | Eif (e1, e2, e3) -> { e with desc = Eif (eval1 e1, e2, e3) }
  | Eapp ({ desc = Eabs (x, _, e11); _ }, v2) when isval v2 -> subst x v2 e11
  | Eapp (v1, e2) when isval v1 -> { e with desc = Eapp (v1, eval1 e2) }
  | Eapp (e1, e2) -> { e with desc = Eapp (eval1 e1, e2) }
  | _ -> raise NoRuleApplies

let rec eval n e =
  if n <= 0 then (false, e)
  else match eval1 e with
    | e' -> eval (n - 1) e'
    | exception NoRuleApplies -> (true, e)
