open Format
open Misc
open Slap.D

(** {2 Output} *)

let logf ?(ppf = std_formatter) fmt =
  let t = Sys.time () in
  let n = truncate t in
  let secs = mod_float t 60.0 in
  let mins = (n / 60) mod 60 in
  let hours = n / 3600 in
  fprintf ppf "%d:%02d:%02f: " hours mins secs;
  fprintf ppf fmt

let output fname f =
  let oc = open_out fname in
  let ppf = formatter_of_out_channel oc in
  f ppf;
  pp_print_flush ppf ();
  close_out oc

let output_samples fname samples =
  output fname
    (fun ppf ->
       List.iter
         (fprintf ppf "%a ;;@\n@\n" (Lambda.pp_lambda pp_print_string))
         samples)

let output_prediction ~pp_type ~unifiable fname test_set preds =
  let pp_type_pair bindings ppf (t, u) =
    let eq = unifiable bindings t u in
    fprintf ppf "%a %s %a" pp_type t (if eq then "==" else "!=") pp_type u
  in
  let combine e1 e2 = Lambda.map2_type (fun t1 t2 -> (t1, t2)) e1 e2 in
  let pp_var = pp_print_string in
  output fname
    (fun ppf ->
       List.map2 combine test_set preds
       |> List.iter (fun e ->
           fprintf ppf "%a@\n--@\n%a@\n@\n"
             (Lambda.pp_lambda pp_var) e
             (Lambda.pp pp_var (pp_type_pair (ref []))) e))

(** {2 Statistics} *)

let stat_features ~dim ff samples =
  let open Lambda in
  let x = Vec.make0 dim in
  let rec aux e =
    axpy (ff e) x;
    match e.desc with
    | Etrue | Efalse | Evar _ -> ()
    | Eabs (_, _, e1) -> aux e1
    | Eapp (e1, e2) -> aux e1 ; aux e2
    | Eif (e1, e2, e3) -> aux e1 ; aux e2 ; aux e3
  in
  List.iter aux samples;
  x

let stat_types ~types ~unifiable samples =
  let aux t1 ns =
    List.map2
      (fun t2 n -> if unifiable (ref []) t1 t2 then succ n else n)
      types ns
  in
  List.fold_left
    (flip (Lambda.fold_type aux)) (List.map (fun _ -> 0) types) samples

(** {2 Loading samples} *)

(** [parse_lambda lexbuf] returns a lambda expression loaded from [lexbuf].
    @raise End_of_file if all expressions are loaded. *)
let parse_lambda lexbuf =
  try LambdaParser.expr LambdaLexer.token lexbuf
  with Parsing.Parse_error ->
    let open Lexing in
    let bp = lexeme_start_p lexbuf in
    let ep = lexeme_end_p lexbuf in
    failwith (Format.asprintf "Parse error near %s:%d:%d-%d"
                bp.pos_fname bp.pos_lnum
                (bp.pos_cnum - bp.pos_bol)
                (ep.pos_cnum - bp.pos_bol))

let load_lambdas fnames =
  let rec load_lines acc lexbuf =
    match parse_lambda lexbuf with
    | e -> load_lines (e :: acc) lexbuf
    | exception End_of_file ->  acc
  in
  let load_file acc fname =
    let ic = open_in fname in
    let lexbuf = Lexing.from_channel ic in
    let acc' = load_lines acc lexbuf in
    close_in ic ; acc'
  in
  List.fold_left load_file [] fnames

let acceptable ~types ~unifiable e =
  let bindings = ref [] in
  let aux t =
    try ignore (List.find (unifiable bindings t) types) ; true
    with Not_found -> false
  in
  Lambda.for_all_type aux e

(** {2 Training-evalutation loop} *)

exception Divergence

let total_f1 smat =
  List.fold_left
    (fun acc row -> acc +. may_default 0.0 (fun (_, s) -> s.Crf.f1) row)
    0.0 smat

let train ~ppf ~pp_type ~max_loops optimize eval target_tr target_te w0 =
  let open Optimize in
  let hook w dJ_dw (i, best_f1, best_w, best_ev) =
    if i > max_loops then Quit (best_w, best_ev) (* timeout *)
    else begin
      logf "Loop #%d: w = [%a]@." i Slap.Io.pp_rfvec w;
      logf ~ppf "Loop #%d: w = [%a]@." i Slap.Io.pp_rfvec w;
      let ((_, _, accuracy, cmat, smat) as ev) = eval w in
      let f1 = total_f1 smat in
      logf "accuracy = %g, F1 = %g@." accuracy f1;
      logf ~ppf "accuracy = %g, F1 = %g@\n\
                 Confusion matrix:@\n%a@\n\
                 Score matrix:@\n%a@."
        accuracy f1
        (Crf.pp_confusion_matrix pp_type) cmat
        (Crf.pp_score_matrix pp_type) smat;
      let y_tr = target_tr w in
      logf "target(training) = %g@." y_tr;
      logf ~ppf "target(training) = %g@." y_tr;
      let y_te = target_te w in
      logf "target(test) = %g@\n" y_te;
      logf ~ppf "target(test) = %g@." y_te;
      (*if 1.0 -. accuracy < 1e-6 then Quit (w, ev)
        else*)
      if best_f1 < f1
      then Continue (i + 1, f1, copy ~y:best_w w, ev)
      else Continue (i + 1, best_f1, best_w, best_ev)
    end
  in
  optimize w0 hook (1, -1.0, Vec.make0 (Vec.dim w0), Obj.magic ())

(** {2 Cross validation} *)

(** [partition n xs] creates [n] disjoint lists from list [xs]. *)
let partition n xs =
  let rec init acc i = if i >= n then acc else init ([] :: acc) (i + 1) in
  let rec aux p1 p2 xs = match p2, xs with
    | _, [] -> List.rev_append p1 p2
    | [], xs -> aux [] (List.rev p1) xs
    | h :: t, x :: xs -> aux ((x :: h) :: p1) t xs
  in
  aux [] (init [] 0) xs

let mkdir dname perm = try Unix.mkdir dname perm with _ -> ()

let main
    ~pp_type ~dim ~cv ~types ~max_loops ~unifiable
    ?(dname = ".") ?(init = fun () -> Vec.make0 dim)
    optimize target ff samples =
  let eval test_set w =
    let preds = List.map
        (Lambda.untyping
         >> Crf.infer ~types (Crf.vertex_log_potential ff w)
         >> fst) test_set in
    let ans = List.map2
        (fun e0 e1 ->
           let e2 = SimpleType.typing_with_hints [] e0 in
           if acceptable ~types ~unifiable:SimpleType.unifiable e2
           then e2 else e1)
        preds test_set in
    let cmat = List.combine ans preds
               |> Crf.confusion_matrix ~types ~unifiable in
    let smat = Crf.score_matrix cmat in
    (ans, preds, Crf.accuracy cmat, cmat, smat)
  in
  let sum_w = Vec.make0 dim in
  let rec aux i parts1 = function
    | [] -> ()
    | p :: parts2 ->
      let dir = dname ^ "/cv" ^ string_of_int i in
      mkdir dir 0o755;
      let oc = open_out (dir ^ "/train.log") in
      let ppf = Format.formatter_of_out_channel oc in
      let test_set = p in
      let training_set = List.rev_append parts1 parts2
                         |> List.fold_left List.rev_append [] in
      output_samples (dir ^ "/training_set.lam") training_set;
      output_samples (dir ^ "/test_set.lam") test_set;
      logf "Cross validation %d/%d@\n\
            training set = %d terms, test set = %d terms@\n\
            [training (#fea)] %a@\n\
            [training (#types)] %a@\n\
            [test (#fea)] %a@\n\
            [test (#types)] %a@."
        i cv (List.length training_set) (List.length test_set)
        Slap.Io.pp_rfvec (stat_features ~dim ff training_set)
        pp_int_list (stat_types ~types ~unifiable training_set)
        Slap.Io.pp_rfvec (stat_features ~dim ff test_set)
        pp_int_list (stat_types ~types ~unifiable test_set);
      let (w, (ans, preds, accuracy, cmat, smat)) =
        train ~ppf ~pp_type ~max_loops
          (optimize training_set) (eval test_set)
          (target training_set) (target test_set)
          (init ()) in
      axpy w sum_w;
      logf "@[w = @[[%a]@]@\nAccuracy = %g@\n%a@\n%a@]@."
        Slap.Io.pp_rfvec w
        accuracy
        (Crf.pp_confusion_matrix pp_type) cmat
        (Crf.pp_score_matrix pp_type) smat;
      output (dir ^ "/weights.txt") (fun ppf -> Slap.Io.pp_rfvec ppf w);
      output_prediction ~pp_type ~unifiable
        (dir ^ "/prediction.txt") ans preds;
      aux (i + 1) (p :: parts1) parts2
  in
  samples
  |> List.map
    (Lambda.alpha
     >> Lambda.map_variable (fun (_, i) -> "x" ^ string_of_int i))
  |> partition cv
  |> aux 1 [];
  scal (1.0 /. float cv) sum_w;
  printf "mean w = [@[%a@]]@." Slap.Io.pp_rfvec sum_w;
