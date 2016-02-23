open Slap.D
open Lambda
open SimpleType

module M : ModelLoader.S =
struct
  module VDim = (val Slap.Size.of_int_dyn 22 : Slap.Size.SIZE)

  type dim = VDim.n
  let dim = VDim.value

  let types =
    [
      Tbool;
      Tarrow (Tbool, Tbool);
      Tarrow (Tarrow (Tbool, Tbool), Tbool);
      Tarrow (Tbool, Tarrow (Tbool, Tbool));
      Tarrow (Tarrow (Tbool, Tbool), Tarrow (Tbool, Tbool));
    ]

  let feature e =
    let x = Vec.make0 VDim.value in
    let put i = Vec.set_dyn x i 1.0 in
    let ( = ) t u = unifiable (ref []) t u in
    let ( <> ) t u = not (t = u) in
    begin match e with
      | { desc = Evar _; _ } -> ()
      | { desc = Etrue; typ = t; } ->
        if t = Tbool then put 1; (* correct *)
        if t <> Tbool then put 2; (* wrong *)
      | { desc = Efalse; typ = t; } ->
        if t = Tbool then put 3; (* correct *)
        if t <> Tbool then put 4; (* wrong *)
      | { desc = Eabs (_, t1, { typ = t2; _ }); typ = t } ->
        if t = Tarrow (t1, t2) && t1 = Tbool && t2 = Tbool
        then put 5; (* strong *)
        if t = Tarrow (t1, t2) && t1 <> Tbool && t2 = Tbool
        then put 6; (* strong *)
        if t = Tarrow (t1, t2) && t1 = Tbool && t2 <> Tbool
        then put 7; (* strong *)
        if t = Tarrow (t1, t2) && t1 <> Tbool && t2 <> Tbool
        then put 8; (* strong *)
        if t1 = Tarrow (t2, t) then put 9; (* wrong *)
        if t2 = Tarrow (t1, t) then put 10; (* wrong *)
      | { desc = Eapp ({ typ = t1; _ }, { typ = t2; _ }); typ = t } ->
        if t1 = Tarrow (t2, t) && t = Tbool && t2 = Tbool
        then put 11; (* strong *)
        if t1 = Tarrow (t2, t) && t <> Tbool && t2 = Tbool
        then put 12; (* strong *)
        if t1 = Tarrow (t2, t) && t = Tbool && t2 <> Tbool
        then put 13; (* strong *)
        if t1 = Tarrow (t2, t) && t <> Tbool && t2 <> Tbool
        then put 14; (* strong *)
        if t1 = Tarrow (t2, t) then put 15; (* strong *)
        if t = Tarrow (t1, t2) then put 16; (* wrong *)
        if t2 = Tarrow (t1, t) then put 17; (* wrong *)
      | { desc = Eif ({ typ = t1; _ },
                      { typ = t2; _ },
                      { typ = t3; _ });
          typ = t } ->
        if t1 = Tbool && t = t2 && t = t3 && t = Tbool
        then put 18; (* correct *)
        if t1 = Tbool && t = t2 && t = t3 && t <> Tbool
        then put 19; (* correct *)
        if t1 <> Tbool then put 20; (* wrong *)
        if t <> t2 then put 21; (* wrong *)
        if t <> t3 then put 22; (* wrong *)
    end;
    x
end

let () =
  ModelLoader.model := Some (module M : ModelLoader.S)
