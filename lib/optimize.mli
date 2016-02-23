open Slap.D

type ('a, 'b) opt_loop = Continue of 'a | Quit of 'b

exception Divergence

(** {2 Gradient ascent} *)

val grad_ascent :
  ?eta:float ->
  ?dec_eta:float ->
  (('a, 'b) Lambda.t list -> ('n, 'cd) vec -> ('n, 'grad_cd) vec) ->
  ('a, 'b) Lambda.t list ->
  ('n, 'cd) vec ->
  (('n, 'cd) vec -> ('n, 'grad_cd) vec -> 'acc -> ('acc, 'ret) opt_loop) ->
  'acc -> 'ret
(*
val sgd :
  rng:Gsl.Rng.t ->
  ?batch_size:int ->
  ?eta:float ->
  ?dec_eta:float ->
  (('a, 'b) Lambda.t list -> ('n, 'cd) vec -> ('n, _) vec) ->
  ('a, 'b) Lambda.t list ->
  ('n, 'cd) vec ->
  (('n, 'cd) vec -> 'acc -> ('acc, 'ret) opt_loop) ->
  'acc -> 'ret

val sgd_ada_grad :
  rng:Gsl.Rng.t ->
  ?batch_size:int ->
  ?eta:float ->
  ?dec_eta:float ->
  (('a, 'b) Lambda.t list -> ('n, 'cd) vec -> ('n, _) vec) ->
  ('a, 'b) Lambda.t list ->
  ('n, 'cd) vec ->
  (('n, 'cd) vec -> 'acc -> ('acc, 'ret) opt_loop) ->
  'acc -> 'ret
*)
