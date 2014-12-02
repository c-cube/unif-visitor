
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

(** {1 Print Terms} *)

module type S = sig
  type symbol (* abstract *)
  type var (* abstract *)

  module T : Term.S

  class printer : object
    inherit [unit] T.visitor
    method print : T.t -> unit -> unit
    method print' : T.t -> unit -> unit
  end

  val print : Format.formatter -> printer
  (** Open recursion printer. One can overload its methods. Upon visit
      it prints the term on the formatter *)
end

module Make(Sym : Symbol.S)(V : Var.S)(T : Term.S with type var = V.t and type symbol = Sym.t)
  : S with module T = T and type var = V.t and type symbol = Sym.t
  = struct

  type symbol = Sym.t
  type var = V.t

  module T = T
  type term = T.t

  let is_composite =
    let o = object(self)
      inherit [bool] T.visitor
      method var _ _ = false
      method const _ _ = false
      method app _ f args = match args with
        | [] -> self#visit f
        | _ -> true
    end
    in o#visit

  let rec pp_list ~sep pp fmt l = match l with
    | [] -> ()
    | [x] -> pp fmt x
    | x::tail ->
        pp fmt x;
        Format.pp_print_string fmt sep;
        Format.pp_print_cut fmt ();
        pp_list ~sep pp fmt tail

  let print fmt = object(self)
    inherit [unit] T.visitor
    method print t () = self#visit t
    method print' t () =
      if is_composite t
      then Format.fprintf fmt "(%t)" (self#print t)
    method var _ v = V.pp fmt v
    method const _ s = Sym.pp fmt s
    method app _ f args = match args with
      | [] -> self#visit f
      | [x] ->
          Format.fprintf fmt "(%t %t)" (self#print' f) (self#print' x)
      | _ ->
          Format.fprintf fmt "(%t %t)" (self#print' f) (pp_list ~sep:" " self#print' args)
  end
end
