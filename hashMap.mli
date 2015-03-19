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

module Poly : sig
  type ('k, 'v) t
  val empty    : unit -> ('k, 'v) t
  val make     : int -> ('k, 'v) t
  val length   : ('k, 'v) t -> int
  val clear    : ('k, 'v) t -> unit
  val copy     : ('k, 'v) t -> ('k, 'v) t
  val add      : 'k -> 'v -> ('k, 'v) t -> unit
  val get      : 'k -> ('k, 'v) t -> 'v
  val set      : 'k -> 'v -> ('k, 'v) t -> unit
  val erase    : 'k -> ('k, 'v) t -> unit
  val find     : 'k -> ('k, 'v) t -> 'v option
  val find_all : 'k -> ('k, 'v) t -> 'v list
  val exist    : 'k -> ('k, 'v) t -> bool
  val iter     : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
  val fold     : ('k -> 'v -> 'a -> 'a) -> ('k, 'v) t -> 'a -> 'a
end

module type Elt = sig 
  type k 
  type v 
end

module Make : functor (E : Elt) -> sig
  type k = E.k
  type v = E.v
  type t = (k, v) Poly.t
  val empty    : unit -> t
  val make     : int -> t
  val length   : t -> int
  val clear    : t -> unit
  val copy     : t -> t
  val add      : k -> v -> t -> unit
  val get      : k -> t -> v
  val set      : k -> v -> t -> unit
  val erase    : k -> t -> unit
  val find     : k -> t -> v option
  val find_all : k -> t -> v list
  val exist    : k -> t -> bool
  val iter     : (k -> v -> unit) -> t -> unit
  val fold     : (k -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

type ('k, 'v) t = ('k, 'v) Poly.t
include module type of Poly with type ('k, 'v) t := ('k, 'v) Poly.t
