module type S =
sig
  type dim

  val dim : dim Slap.Size.t

  val types : SimpleType.t list

  val feature : ('a, SimpleType.t) Lambda.t -> (dim, 'cnt) Slap.D.vec
end

let model : ((module S) option) ref = ref None
