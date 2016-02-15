type t =
  | Tun (* untypable *)
  | Tbool
  | Tarrow of t * t
  | Tvar of t option ref (** for destructive unification *)

let string_of_tvar =
  let tbl = ref [] in
  fun r ->
    try List.assq r !tbl
    with Not_found ->
      let s = "'a" ^ string_of_int (List.length !tbl) in
      tbl := (r, s) :: !tbl ; s

let pp =
  let open Format in
  let rec aux b ppf = function
    | Tun -> pp_print_string ppf "Un"
    | Tbool -> pp_print_string ppf "Bool"
    | Tvar ({ contents = None } as r) -> pp_print_string ppf (string_of_tvar r)
    | Tvar { contents = Some t } -> aux b ppf t
    | Tarrow (t1, t2) ->
      let (fmt : _ format) = if b then "(%a -> %a)" else "%a -> %a" in
      fprintf ppf fmt (aux true) t1 (aux false) t2
  in
  aux false

let rec equal t u = match t, u with
  | Tun, Tun | Tbool, Tbool -> true
  | Tvar r1, Tvar r2 when r1 == r2 -> true
  | Tvar { contents = Some t }, u
  | t, Tvar { contents = Some u } -> equal t u
  | Tarrow (t1, t2), Tarrow (u1, u2) -> equal t1 u1 && equal t2 u2
  | _ -> false

let rec simplify = function
  | Tun -> Tun
  | Tbool -> Tbool
  | Tarrow (t1, t2) -> Tarrow (simplify t1, simplify t2)
  | Tvar ({ contents = None } as r) -> Tvar r
  | Tvar { contents = Some t } -> simplify t

let unifiable bindings =
  let rec occurs_check r1 = function
    | Tun | Tbool -> true
    | Tvar r2 when r1 == r2 -> false
    | Tvar ({ contents = None } as r2) ->
      begin match List.assq r2 !bindings with
        | t2 -> occurs_check r1 t2
        | exception Not_found -> true
      end
    | Tvar { contents = Some t2 } -> occurs_check r1 t2
    | Tarrow (t2, u2) -> occurs_check r1 t2 && occurs_check r1 u2
  in
  let rec aux t1 t2 = match t1, t2 with
    | Tun, Tun | Tbool, Tbool -> true
    | Tarrow (t1, u1), Tarrow (t2, u2) -> aux t1 t2 && aux u1 u2
    | Tvar r1, Tvar r2 when r1 == r2 -> true
    | Tvar { contents = Some t1 }, t2
    | t1, Tvar { contents = Some t2 } -> aux t1 t2
    | Tvar ({ contents = None } as r), t
    | t, Tvar ({ contents = None } as r) ->
      begin
        try aux (List.assq r !bindings) t
        with Not_found ->
          if occurs_check r t
          then begin bindings := (r, t) :: !bindings ; true end
          else false
      end
    | _ -> false
  in
  aux

let rec occurs_check r1 = function
  | Tun | Tbool -> true
  | Tvar r2 when r1 == r2 -> false
  | Tvar { contents = None } -> true
  | Tvar { contents = Some t2 } -> occurs_check r1 t2
  | Tarrow (t2, u2) -> occurs_check r1 t2 && occurs_check r1 u2

exception Type_error

let rec unify t1 t2 = match t1, t2 with
  | Tun, Tun | Tbool, Tbool -> ()
  | Tarrow (t1, u1), Tarrow (t2, u2) -> unify t1 t2; unify u1 u2
  | Tvar r1, Tvar r2 when r1 == r2 -> ()
  | Tvar { contents = Some t1 }, _ -> unify t1 t2
  | _, Tvar { contents = Some t2 } -> unify t1 t2
  | Tvar ({ contents = None } as r1), _ when occurs_check r1 t2 ->
    r1 := Some t2
  | _, Tvar ({ contents = None } as r2) when occurs_check r2 t1 ->
    r2 := Some t1
  | _ -> raise Type_error

let pp' = pp

let rec typing ctx e =
  let open Lambda in
  match e.desc with
  | Etrue -> { typ = Tbool; desc = Etrue }
  | Efalse -> { typ = Tbool; desc = Efalse }
  | Evar x -> { typ = List.assoc x ctx; desc = Evar x }
  | Eabs (x, (), e2) ->
    let t = Tvar (ref None) in
    let e2 = typing ((x, t) :: ctx) e2 in
    let desc = Eabs (x, t, e2) in
    if equal t Tun || equal e2.typ Tun then { typ = Tun; desc; }
    else { typ = Tarrow (t, e2.typ); desc; }
  | Eapp (e1, e2) ->
    let e1 = typing ctx e1 in
    let e2 = typing ctx e2 in
    let desc = Eapp (e1, e2) in
    let t = Tvar (ref None) in
    if unifiable (ref []) e1.typ (Tarrow (e2.typ, t)) &&
       not (equal e2.typ Tun)
    then begin
      unify e1.typ (Tarrow (e2.typ, t));
      { typ = t; desc; }
    end else { typ = Tun; desc; }
  | Eif (e1, e2, e3) ->
    let e1 = typing ctx e1 in
    let e2 = typing ctx e2 in
    let e3 = typing ctx e3 in
    let desc = Eif (e1, e2, e3) in
    let bindings = ref [] in
    if unifiable bindings e1.typ Tbool &&
       unifiable bindings e2.typ e3.typ &&
       not (equal e2.typ Tun) && not (equal e3.typ Tun)
    then begin
      unify e1.typ Tbool;
      unify e2.typ e3.typ;
      { typ = e2.typ; desc; }
    end else { typ = Tun; desc; }

let typing_with_hints ctx e =
  let open Lambda in
  let rec aux ctx e = match e.desc with
    | Etrue -> { typ = Tbool; desc = Etrue }
    | Efalse -> { typ = Tbool; desc = Efalse }
    | Evar x -> { typ = List.assoc x ctx; desc = Evar x }
    | Eabs (x, t, e2) ->
      let t = if t = Tun then Tvar (ref None) else t in
      let e2 = aux ((x, t) :: ctx) e2 in
      let desc = Eabs (x, t, e2) in
      if equal t Tun || equal e2.typ Tun then { typ = Tun; desc; }
      else { typ = Tarrow (t, e2.typ); desc; }
    | Eapp (e1, e2) ->
      let e1 = aux ctx e1 in
      let e2 = aux ctx e2 in
      let desc = Eapp (e1, e2) in
      let t = Tvar (ref None) in
      if unifiable (ref []) e1.typ (Tarrow (e2.typ, t)) &&
         not (equal e2.typ Tun)
      then begin
        unify e1.typ (Tarrow (e2.typ, t));
        { typ = t; desc; }
      end else { typ = Tun; desc; }
    | Eif (e1, e2, e3) ->
      let e1 = aux ctx e1 in
      let e2 = aux ctx e2 in
      let e3 = aux ctx e3 in
      let desc = Eif (e1, e2, e3) in
      let bindings = ref [] in
      if unifiable bindings e1.typ Tbool &&
         unifiable bindings e2.typ e3.typ &&
         not (equal e2.typ Tun) && not (equal e3.typ Tun)
      then begin
        unify e1.typ Tbool;
        unify e2.typ e3.typ;
        { typ = e2.typ; desc; }
      end else { typ = Tun; desc; }
  in
  aux ctx e |> map_type simplify

let rec remove_tvars u = function
  | Tun -> Tun | Tbool -> Tbool
  | Tvar _ -> u
  | Tarrow (t1, t2) -> Tarrow (remove_tvars u t1, remove_tvars u t2)
