(*
 * Chibisat - a tiny DPLL based SAT Solver 
 * Copyright (c) 2013 Takahisa Watanabe <linerlock@outlook.com> All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

let default_capacity = ref 256

module Poly = struct
  type ('k, 'v) t = ('k, 'v) Hashtbl.t

  let empty () = 
    Hashtbl.create !default_capacity

  let make size =
    Hashtbl.create size
  
  let length hashMap =
    Hashtbl.length hashMap

  let copy hashMap =
    Hashtbl.copy hashMap

  let clear hashMap =
    Hashtbl.clear hashMap

  let add k v hashMap =
    Hashtbl.add hashMap k v
  
  let get k hashMap =
    Hashtbl.find hashMap k

  let set k v hashMap =
    Hashtbl.replace hashMap k v

  let erase k hashMap =
    Hashtbl.remove hashMap k

  let find k hashMap =
    try
      Some (Hashtbl.find hashMap k)
    with
      Not_found -> None

  let find_all k hashMap =
    Hashtbl.find_all hashMap k

  let exist k hashMap =
    Hashtbl.mem hashMap k

  let iter f hashMap =
    Hashtbl.iter f hashMap

  let fold f hashMap =
    Hashtbl.fold f hashMap
end

module type Elt = sig 
  type k
  type v
end

module Make (E : Elt) = struct
  type k = E.k
  type v = E.v
  type t = (k, v) Poly.t

  let empty    = Poly.empty
  let make     = Poly.make
  let length   = Poly.length
  let clear    = Poly.clear
  let copy     = Poly.copy
  let add      = Poly.add
  let get      = Poly.get
  let set      = Poly.set
  let erase    = Poly.erase
  let find     = Poly.find
  let find_all = Poly.find_all
  let exist    = Poly.exist
  let iter     = Poly.iter
  let fold     = Poly.fold
end

include Poly
