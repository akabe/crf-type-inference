open Slap.D
open Lambda
open SimpleType

module M : ModelLoader.S =
struct
  module VDim = (val Slap.Size.of_int_dyn 14 : Slap.Size.SIZE)

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
    let ( = ) t u = equal t u in
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
        if t = Tarrow (t1, t2) then put 5; (* correct *)
        if t1 = Tarrow (t2, t) then put 6; (* wrong *)
        if t2 = Tarrow (t1, t) then put 7; (* wrong *)
      | { desc = Eapp ({ typ = t1; _ }, { typ = t2; _ }); typ = t } ->
        if t1 = Tarrow (t2, t) then put 8; (* correct *)
        if t = Tarrow (t1, t2) then put 9; (* wrong *)
        if t2 = Tarrow (t1, t) then put 10; (* wrong *)
      | { desc = Eif ({ typ = t1; _ },
                      { typ = t2; _ },
                      { typ = t3; _ });
          typ = t } ->
        if t1 = Tbool && t = t2 && t = t3 then put 11; (* correct *)
        if t1 <> Tbool then put 12; (* wrong *)
        if t <> t2 then put 13; (* wrong *)
        if t <> t3 then put 14; (* wrong *)
    end;
    x
end

let () =
  ModelLoader.model := Some (module M : ModelLoader.S)
