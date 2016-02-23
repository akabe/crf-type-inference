open Format
open Crf
open Utils
open Misc
open Lambda
open Slap.D

let fontname = "DejaVuSansMono"
let fontsize = 20

let pp_dot ff w ppf e =
  let genid =
    let c = ref (-1) in
    fun () -> incr c ; !c
  in
  let rec aux ctx e =
    let lp = Crf.vertex_log_potential ff w e in
    match e.desc with
    | Evar x -> List.assoc x ctx
    | Etrue ->
      let id = genid () in
      fprintf ppf "v%d [label=\"true : %a\\n(%g)\", \
                   fontname=\"%s\", fontsize=%d]@\n"
        id SimpleType.pp e.typ lp fontname fontsize;
      id
    | Efalse ->
      let id = genid () in
      fprintf ppf "v%d [label=\"false : %a\\n(%g)\", \
                   fontname=\"%s\", fontsize=%d]@\n"
        id SimpleType.pp e.typ lp fontname fontsize;
      id
    | Eabs (x, t, e2) ->
      let id = genid () in
      let id1 = genid () in
      let lp1 = Crf.vertex_log_potential ff w { desc = Evar x; typ = t } in
      let id2 = aux ((x, id1) :: ctx) e2 in
      fprintf ppf "v%d [label=\"abs : %a\\n(%g)\", \
                   fontname=\"%s\" fontsize=%d]@\n\
                   v%d [label=\"var(%s) : %a\\n(%g)\", \
                   fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"arg\", fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"body\", fontname=\"%s\", fontsize=%d]@\n"
        id SimpleType.pp e.typ lp fontname fontsize
        id1 x SimpleType.pp t lp1 fontname fontsize
        id id1 fontname fontsize id id2 fontname fontsize;
      id
    | Eapp (e1, e2) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      fprintf ppf "v%d [label=\"app : %a\\n(%g)\", \
                   fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"fun\", fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"arg\", fontname=\"%s\", fontsize=%d]@\n"
        id SimpleType.pp e.typ lp fontname fontsize
        id id1 fontname fontsize id id2 fontname fontsize;
      id
    | Eif (e1, e2, e3) ->
      let id = genid () in
      let id1 = aux ctx e1 in
      let id2 = aux ctx e2 in
      let id3 = aux ctx e3 in
      fprintf ppf "v%d [label=\"if : %a\\n(%g)\", \
                   fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"cond\", fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"then\", fontname=\"%s\", fontsize=%d]@\n\
                   v%d -> v%d [label=\"else\", fontname=\"%s\", fontsize=%d]@\n"
        id SimpleType.pp e.typ lp fontname fontsize
        id id1 fontname fontsize
        id id2 fontname fontsize
        id id3 fontname fontsize;
      id
  in
  fprintf ppf "digraph lambda {@\n\
               size=\"100,100\";@\n";
  ignore (aux [] e);
  pp_print_string ppf "}\n"

let show_graph ~fname ff w e =
  let open Unix in
  let oc = open_process_out ("dot -Tsvg -o " ^ fname) in
  let ppf = formatter_of_out_channel oc in
  fprintf ppf "%a@." (pp_dot ff w) e;
  match close_process_out oc with
  | WEXITED 0 -> ignore (system ("eog " ^ fname ^ " &"))
  | WEXITED n -> eprintf "dot command exited at status %d.@." n
  | _ -> eprintf "dot command was killed.@."

let make_lexbuf () =
  let open Lexing in
  let lexbuf = from_channel Pervasives.stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "toplevel" };
  lexbuf

let main ~types ff w =
  let lexbuf = make_lexbuf () in
  print_endline "Ctrl+D for exit.";
  try
    while true do
      print_string "> ";
      print_flush ();
      try
        let e0 = parse_lambda lexbuf in (* an untyped lambda expression *)
        let (e1, lp) = Crf.infer ~types (Crf.vertex_log_potential ff w) e0 in
        let p = exp lp in (* potential *)
        let z = Crf.normalizer ~types ff w e1 in
        printf "log potential = %g@\n\
                potential = %g@\n\
                probability = %g@." lp p (p *. z);
        show_graph ~fname:"lambda.svg" ff w e1
      with Failure msg -> eprintf "Error: %s@." msg
    done
  with End_of_file -> print_endline ""

let load_weights fname n =
  let re = Str.regexp "[ \t]+" in
  let ic = open_in fname in
  let w = input_line ic
          |> Str.split re
          |> filter_map (maybe float_of_string) in
  close_in ic;
  Vec.of_list_dyn n w

let () =
  if Array.length Sys.argv < 3
  then eprintf "Usage: %s EXP_DIR RES_DIR@." Sys.argv.(0)
  else begin
    let exp_dir = Sys.argv.(1) in
    let res_dir = Sys.argv.(2) in
    let model_file = exp_dir ^ "/model.cmxs" in
    let weights_file = res_dir ^ "/weights.txt" in
    (* Generate weights.txt if it is doesn't exits. *)
    if not (Sys.file_exists weights_file) then begin
      Sys.command (asprintf "grep 'mean w' %s/stdout.log \
                             | sed 's/mean w = \\[\\(.*\\)\\]/\\1/' \
                             > %s/weights.txt"
                     res_dir res_dir)
      |> ignore
    end;
    try
      Dynlink.loadfile model_file;
      match !ModelLoader.model with
      | Some model ->
        let module Model = (val model : ModelLoader.S) in
        let w = load_weights weights_file Model.dim in
        printf "w = @[%a@]@." Slap.Io.pp_rfvec w;
        main ~types:Model.types Model.feature w
      | None ->
        eprintf "Load failed: %s@." model_file
    with
    | Dynlink.Error e ->
      eprintf "%s@." (Dynlink.error_message e)
  end
