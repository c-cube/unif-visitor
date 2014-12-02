
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

(** {1 Unary Unification} *)

module type S = sig
  type term
  type var
  type subst

  val unif : ?subst:subst -> term -> term -> subst option
end

module Make
  (Sym : Symbol.S)
  (V : Var.S)
  (T : Term.S with type var = V.t and type symbol = Sym.t)
  (Su : Subst.S with type var = V.t and type term = T.t)
  : S with type subst = Su.t
    and type term = T.t
    and type var = V.t
  = struct
  
  type term = T.t
  type var = V.t
  type subst = Su.t

  class eval_visitor subst = object(self)
    inherit [term] T.visitor
    method var t v = match Su.get subst v with
      | None -> t
      | Some t' -> self#visit t'
    method app t _ _ = t
    method const t _ = t
  end

  (* evaluate the term if it's a variable *)
  let eval subst t = (new eval_visitor subst)#visit t

  (* occur check *)
  let occur_check subst v =
    let visitor = object(self)
      inherit [bool] T.visitor
      method var _ v' = match Su.get subst v' with
        | None -> V.compare v v' = 0
        | Some t' -> self#visit t'
      method app _ f args =
        self#visit f || List.exists self#visit args
      method const _ _ = false
    end in
    visitor#visit

  exception Fail

  let unif ?(subst=Su.empty) t1 t2 =
    (* unify two terms *)
    let rec unif subst t1 t2 =
      let t1 = eval subst t1 in
      let t2 = eval subst t2 in
      (unif_visitor_left subst t2)#visit t1
    (* match first term *)
    and unif_visitor_left subst t2 = object
      inherit [subst] T.visitor
      method var _ v =
        if occur_check subst v t2 then raise Fail;
        Su.bind subst v t2
      method const _ s =
        let o = object
          inherit [subst] T.visitor
          method var _ _ = raise Fail
          method app _ _ _ = raise Fail
          method const _ s' =
            if Sym.compare s s' = 0 then subst else raise Fail
        end in
        o#visit t2
      method app _ f args =
        let o = object 
          inherit [subst] T.visitor
          method var _ _ = raise Fail
          method const _ _ = raise Fail
          method app _ f' args' =
            if List.length args <> List.length args' then raise Fail;
            let subst = unif subst f f' in
            List.fold_left2 unif subst args args'
        end in
        o#visit t2
    end in
    try Some (unif subst t1 t2)
    with Fail -> None
end
