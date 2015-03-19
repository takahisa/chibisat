(*
 * Chibisat
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
  type 'a t = {
    mutable elts     : 'a array;
    mutable size     : int;
    mutable capacity : int;
  }
  val empty            : unit -> 'a t
  val make             : int -> 'a -> 'a t
  val size             : 'a t -> int
  val copy             : 'a t -> 'a t
  val clear            : 'a t -> unit
  val of_list          : 'a list -> 'a t
  val to_list          : 'a t -> 'a list
  val nth              : int -> 'a t -> 'a
  val update           : int -> 'a -> 'a t -> unit
  val head             : 'a t -> 'a
  val last             : 'a t -> 'a
  val push             : 'a -> 'a t -> unit
  val pop              : 'a t -> 'a
  val find             : ('a -> bool) -> 'a t -> int option
  val exist            : ('a -> bool) -> 'a t -> bool
  val iteri            : (int -> 'a -> 'b) -> 'a t -> unit
  val iter             : ('a -> 'b) -> 'a t -> unit
  val remove_at        : int -> 'a t -> unit
  val remove           : 'a -> 'a t -> unit
  val fold_left        : ('a -> 'b -> 'a) -> 'b t -> 'a -> 'a
  val fold_right       : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val sequential_equal : 'a t -> 'a t -> bool
end

module type Elt = sig 
  type t 
end

module Make : functor (E : Elt) -> sig
  type elt = E.t
  type t   = E.t Poly.t
  val empty            : unit -> t
  val make             : int -> elt -> t
  val size             : t -> int
  val copy             : t -> t
  val clear            : t -> unit
  val of_list          : elt list -> t
  val to_list          : t -> elt list
  val nth              : int -> t -> elt
  val update           : int -> elt -> t -> unit
  val head             : t -> elt
  val last             : t -> elt
  val push             : elt -> t -> unit
  val pop              : t -> elt
  val find             : (elt -> bool) -> t -> int option
  val exist            : (elt -> bool) -> t -> bool
  val iteri            : (int -> elt -> unit) -> t -> unit
  val iter             : (elt -> unit) -> t -> unit
  val remove_at        : int -> t -> unit
  val remove           : elt -> t -> unit
  val fold_left        : ('a -> elt -> 'a) -> t -> 'a -> 'a
  val fold_right       : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val sequential_equal : t -> t -> bool
end

module Slice : sig
  type 'a t = {
    vector  : 'a Poly.t;
    i       : int;
    n       : int
  }
  
  val make      : int -> int -> 'a Poly.t -> 'a t
  val nth       : int -> 'a t -> 'a
  val find      : ('a -> bool) -> 'a t -> int option
  val exist     : ('a -> bool) -> 'a t -> bool
  val iteri     : (int -> 'a -> unit) -> 'a t -> unit
  val iter      : ('a -> unit) -> 'a t -> unit
  val to_vector : 'a t -> 'a Poly.t
end

type 'a t = 'a Poly.t
include module type of Poly with type 'a t := 'a Poly.t
