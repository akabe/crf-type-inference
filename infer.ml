open Format
open Misc
open Lambda
open SimpleType

module Model =
struct
  let dim = 19

  let types =
    [
      Tbool;
      Tarrow (Tbool, Tbool);
      Tarrow (Tarrow (Tbool, Tbool), Tbool);
      Tarrow (Tbool, Tarrow (Tbool, Tbool));
      Tarrow (Tarrow (Tbool, Tbool), Tarrow (Tbool, Tbool));
    ]

  let weights =
    [|
      0.261888; -0.261888;
      0.267968; -0.267968;
      0.114038; 0.114038; -0.00319744; -0.00319744;
      0.0576576; 0.0576576; -0.00219648; -0.00219648;
      0.259904; 0.259904; 0.259904; 0.254464; -0.259904; -0.259904; -0.259904
    |]

  let feature e =
    let x = Array.make dim 0.0 in
    let put i = x.(i-1) <- 1.0 in
    let ( = ) t u = equal t u in
    let ( <> ) t u = not (t = u) in
    let eqarr1 t = function Tarrow (u, _) -> t = u | _ -> false in
    let eqarr2 t = function Tarrow (_, u) -> t = u | _ -> false in
    begin match e with
      | { desc = Evar _; _ } -> ()
      | { desc = Etrue; typ = t; } ->
        if t = Tbool then put 1; (* correct *)
        if t <> Tbool then put 2; (* wrong *)
      | { desc = Efalse; typ = t; } ->
        if t = Tbool then put 3; (* correct *)
        if t <> Tbool then put 4; (* wrong *)
      | { desc = Eabs (_, t1, { typ = t2; _ }); typ = t } ->
        if eqarr1 t1 t then put 5; (* week *)
        if eqarr2 t2 t then put 6; (* week *)
        if t1 = Tarrow (t2, t) then put 7; (* wrong *)
        if t2 = Tarrow (t1, t) then put 8; (* wrong *)
      | { desc = Eapp ({ typ = t1; _ },
                       { typ = t2; _ }); typ = t } ->
        if eqarr1 t2 t1 then put 9; (* week *)
        if eqarr2 t t1 then put 10; (* week *)
        if t = Tarrow (t1, t2) then put 11; (* wrong *)
        if t2 = Tarrow (t1, t) then put 12; (* wrong *)
      | { desc = Eif ({ typ = t1; _ },
                      { typ = t2; _ },
                      { typ = t3; _ });
          typ = t } ->
        if t1 = Tbool then put 13; (* weak *)
        if t = t2 then put 14; (* weak *)
        if t = t3 then put 15; (* weak *)
        if t2 = t3 then put 16; (* weak *)
        if t1 <> Tbool then put 17; (* wrong *)
        if t <> t2 then put 18; (* wrong *)
        if t <> t3 then put 19; (* wrong *)
    end;
    x
end

let parse str =
  let open Lexing in
  let lexbuf = from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "toplevel" };
  try LambdaParser.expr LambdaLexer.token lexbuf
  with Parsing.Parse_error ->
    let bp = lexeme_start_p lexbuf in
    let ep = lexeme_end_p lexbuf in
    failwith (Format.asprintf "Parse error near %s:%d:%d-%d"
                bp.pos_fname bp.pos_lnum
                (bp.pos_cnum - bp.pos_bol)
                (ep.pos_cnum - bp.pos_bol))

let pp_dot score ppf e =
  let genid =
    let c = ref (-1) in
    fun () -> incr c ; !c
  in
  let rec aux ctx e =
    let lp = score e in
    match e.desc with
    | Evar x -> List.assoc x ctx
    | Etrue ->
      let id = genid () in
      fprintf ppf "v%d [label=\"true : %a\\n(%g)\"]@\n"
        id SimpleType.pp e.typ lp;
      id
    | Efalse ->
      let id = genid () in
      fprintf ppf "v%d [label=\"false : %a\\n(%g)\"]@\n"
        id SimpleType.pp e.typ lp;
      id
    | Eabs (x, t, e2) ->
      let id = genid () in
      let id1 = genid () in
      let lp1 = score { desc = Evar x; typ = t } in
      let id2 = aux ((x, id1) :: ctx) e2 in
      fprintf ppf "v%d [label=\"abs : %a\\n(%g)\"]@\n\
                   v%d [label=\"var(%s) : %a\\n(%g)\"]@\n\
                   v%d -> v%d [label=\"arg\"]@\n\
                   v%d -> v%d [label=\"body\"]@\n"
        id SimpleType.pp e.typ lp id1 x SimpleType.pp t lp1 id id1 id id2;
      id
    | Eapp (e1, e2) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      fprintf ppf "v%d [label=\"app : %a\\n(%g)\"]@\n\
                   v%d -> v%d [label=\"fun\"]@\n\
                   v%d -> v%d [label=\"arg\"]@\n"
        id SimpleType.pp e.typ lp id id1 id id2;
      id
    | Eif (e1, e2, e3) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      let id3 = aux ctx e3 in
      fprintf ppf "v%d [label=\"if : %a\\n(%g)\"]@\n\
                   v%d -> v%d [label=\"cond\"]@\n\
                   v%d -> v%d [label=\"then\"]@\n\
                   v%d -> v%d [label=\"else\"]@\n"
        id SimpleType.pp e.typ lp id id1 id id2 id id3;
      id
  in
  fprintf ppf "digraph lambda {@\n";
  ignore (aux [] e);
  pp_print_string ppf "}\n"

let alert s = Js.Unsafe.eval_string (asprintf "alert(%S);" s)

let () =
  Js.Unsafe.global##.CRFInfer :=
    Js.wrap_callback (fun js_str ->
        let str = Js.to_string js_str ^ ";;" in
        let e0 = parse str in
        let lpf = Crf.vertex_log_potential Model.feature Model.weights in
        let (e1, lp) = Crf.infer ~types:Model.types lpf e0 in
        let z =
          Crf.normalizer ~types:Model.types Model.feature Model.weights e1 in
        let dot = asprintf "%a" (pp_dot lpf) e1 in
        Json.output (dot, lp, z))
