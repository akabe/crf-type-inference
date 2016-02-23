(** Expression of lambda + Bool *)

type ('a, 'b) t = { typ : 'b; desc : ('a, 'b) desc; }
and ('a, 'b) desc =
  | Evar of 'a
  | Eabs of 'a * 'b * ('a, 'b) t
  | Eapp of ('a, 'b) t * ('a, 'b) t
  | Etrue
  | Efalse
  | Eif of ('a, 'b) t * ('a, 'b) t * ('a, 'b) t
  [@@deriving show]

val tru : ('a, unit) t
val fls : ('a, unit) t
val var : 'a -> ('a, unit) t
val lam : 'a -> ('a, unit) t -> ('a, unit) t
val ($) : ('a, unit) t -> ('a, unit) t -> ('a, unit) t
val if_ : ('a, unit) t -> ('a, unit) t -> ('a, unit) t -> ('a, unit) t

val isval : ('a, 'b) t -> bool

val size : ('a, 'b) t -> int

val depth : ('a, 'b) t -> int

val alpha : ('a, 'b) t -> ('a * int, 'b) t

val subst : 'a -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val map_variable : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

val map_type : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

val map2_type : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

val for_all_type : ('b -> bool) -> ('a, 'b) t -> bool

val fold_type : ('b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc

val fold2_type :
  ('b -> 'c -> 'acc -> 'acc) -> ('a, 'b) t -> ('a, 'c) t -> 'acc -> 'acc

val untyping : ('a, 'b) t -> ('a, unit) t

(** {2 Pretty printing} *)

val pp_lambda :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> ('a, 'b) t -> unit

val pp_dot :
  (Format.formatter -> ('a, 'b) t -> unit) ->
  Format.formatter -> ('a, 'b) t -> unit

(** {2 Evaluation} *)

exception NoRuleApplies

(** [eval1 e] one-step evaluates expression [e].
    @raise NoRuleApplies if a given expression cannot be reduced anymore. *)
val eval1 : ('a, 'b) t -> ('a, 'b) t

val eval : int -> ('a, 'b) t -> bool * ('a, 'b) t
