
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Terms} *)

module type S = sig
  type var (* abstract *)
  type symbol (* abstract *)

  type t

  (** Constructors *)

  val var : var -> t
  val const : symbol -> t
  val app : t -> t list -> t

  (** Visitor. First argument of methods is the term itself  *)

  class virtual ['a] visitor : object
    method virtual var : t -> var -> 'a
    method virtual app : t -> t -> t list -> 'a
    method virtual const : t -> symbol -> 'a
    method visit : t -> 'a
  end
end

module Make(V:Var.S)(Sym:Symbol.S)
  : S with type var = V.t and type symbol = Sym.t
  = struct

  type var = V.t
  type symbol = Sym.t

  type t =
    | Const of Sym.t
    | Var of V.t
    | App of t * t list
    [@@deriving ord,show]

  let var v = Var v
  let const s = Const s
  let app f args = match args with
    | [] -> f
    | _ -> App(f,args)

  class virtual ['a] visitor = object(self)
    method virtual var : t -> var -> 'a
    method virtual app : t -> t -> t list -> 'a
    method virtual const : t -> symbol -> 'a
    method visit t = match t with
      | Const s -> self#const t s
      | Var v -> self#var t v
      | App (f,args) -> self#app t f args
  end
end
