
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

(** {1 Symbols} *)

module type S = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

(** Hashconsed Strings *)
module HString : sig
  include S

  val make : string -> t
end = struct
  type t = {
    name : string;
    mutable id : int;
  }

  type sym = t

  module H = Weak.Make(struct
    type t = sym
    let equal {name=n1;_} {name=n2;_} = n1=n2
    let hash {name; _} = Hashtbl.hash name
  end)

  let compare a b = Pervasives.compare a.id b.id

  let pp fmt {name;_} = Format.pp_print_string fmt name

  let table = H.create 128
  let count = ref 0

  let make name =
    let sym = {id= -1; name} in
    let sym' = H.merge table sym in
    if sym == sym' then (
      sym.id <- !count;
      incr count;
    );
    sym'
end
