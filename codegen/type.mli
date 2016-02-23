(** Simple types. *)

type type_var

type t =
  | Tbool
  | Tvar of type_var
  | Tarrow of t * t

type constraint_set = (t * t) list

(* type unifier = (type_var * t) list *)

val genvar : unit -> t

(* val unify : constraint_set -> unifier *)

val unifiable : constraint_set -> bool

(* val typing : (string * t) list -> unit Lambda.t -> t Lambda.t *)

val pp : Format.formatter -> t -> unit

(* val parse : string -> t *)
