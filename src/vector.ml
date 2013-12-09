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

module Poly = struct
  type 'a t = {
    mutable elts     : 'a array;
    mutable size     : int;
    mutable capacity : int
  }
    
  let empty () =
    { elts     = [||];
      size     = 0;
      capacity = 0 }
      
  let make size init =
    if size < 0 then
      raise (Invalid_argument "Index out of range");
    { elts     = Array.make size init;
      size     = size;
      capacity = size }
      
  let size vec =
    vec.size
      
  let copy vec =
    { vec with elts = Array.copy vec.elts }
      
  let clear vec =
    vec.size <- 0
      
  let of_list elts =
    let size = List.length elts in
    { elts     = Array.of_list elts;
      size     = size;
      capacity = size }
      
  let to_list vec =
    Array.to_list (Array.sub vec.elts 0 vec.size)

  let nth i vec =
    if i < 0 || i >= vec.size then
      raise (Invalid_argument "Index out of bounds");
    vec.elts.(i)
      
  let update i v vec =
    if i < 0 || i >= vec.size then
      raise (Invalid_argument "Index out of bounds");
    vec.elts.(i) <- v
      
  let head vec =
    nth 0 vec
      
  let last vec =
    nth (size vec - 1) vec
      
  let grow_to init vec =
    let incremental = 32 in
    vec.elts <- Array.append vec.elts (Array.create incremental init);
    vec.capacity <- vec.capacity + incremental
      
  let push v vec =
    while vec.size >= vec.capacity do
      grow_to v vec
    done;
    vec.elts.(vec.size) <- v;
    vec.size <- vec.size + 1
      
  let pop vec =
    if vec.size <= 0 then
      raise (Invalid_argument "Stack underflow");
    vec.size <- vec.size - 1;
    vec.elts.(vec.size)
      
  let find f vec =
    let rec loop i n =
      if i >= n then
    None
      else if f (nth i vec) then
    Some i
      else
    loop (i + 1) n
    in
    loop 0 vec.size

  let exist f vec =
    match find f vec with
    | Some _ -> true
    | None   -> false
      
  let iteri f vec =
    for i = 0 to vec.size - 1 do
      f i (nth i vec)
    done

  let iter f vec =
    iteri (fun _ v -> f v) vec

  let remove_at i vec =
    if i < 0 || i >= vec.size then
      raise (Invalid_argument "Index out of range");
    if vec.size - 1 = i then
      vec.size <- vec.size - 1
    else
      begin Array.blit vec.elts (i + 1) vec.elts i (vec.size - i - 1);
            vec.size <- vec.size - 1 end

  let remove elt vec =
    match find ((=) elt) vec with
    | Some i -> remove_at i vec
    | None   -> raise Not_found
      
  let fold_left f vec seed =
    let rec loop i acc =
      if i >= vec.size then
    acc
      else
    loop (i + 1) (f acc (nth i vec))
    in
    loop 0 seed

  let fold_right f vec seed =
    let rec loop i acc=
      if i < 0 then
    acc
      else
    loop (i - 1) (f (nth i vec) acc)
    in
    loop (vec.size - 1) seed

  let sequential_equal x y =
    Array.sub x.elts 0 x.size = Array.sub x.elts 0 y.size

end

module type Elt = sig
  type t
end

module Make (E : Elt) = struct
  type elt             = E.t
  type t               = E.t Poly.t
  let empty            = Poly.empty
  let make             = Poly.make
  let size             = Poly.size
  let copy             = Poly.copy
  let clear            = Poly.clear
  let of_list          = Poly.of_list
  let to_list          = Poly.to_list
  let nth              = Poly.nth
  let update           = Poly.update
  let head             = Poly.head
  let last             = Poly.last
  let push             = Poly.push
  let pop              = Poly.pop
  let find             = Poly.find
  let exist            = Poly.exist
  let iteri            = Poly.iteri
  let iter             = Poly.iter
  let remove_at        = Poly.remove_at
  let remove           = Poly.remove
  let fold_left        = Poly.fold_left
  let fold_right       = Poly.fold_right
  let sequential_equal = Poly.sequential_equal
end

module Slice = struct
  type 'a t = {
    vector  : 'a Poly.t;
    i       : int;
    n       : int
  }
  
  let make i n vector =
    if i < 0 || n < 0 || i + n > Poly.size vector then
      raise (Invalid_argument "Index out of range");
    { vector = vector;
      i      = i;
      n      = n }
    
  let nth i slice =
    Poly.nth (slice.i + i) slice.vector
      
  let find f slice =
    let rec loop i n =
      if i >= n then
    None
      else if f (nth i slice) then
    Some i
      else
    loop (i + 1) n
    in
    loop 0 slice.n
      
  let exist f slice =
    let rec loop i n =
      if i >= n then
    false
      else if f (nth i slice) then
    true
      else
    loop (i + 1) n
    in
    loop 0 slice.n
      
  let iteri f slice =
    for i = 0 to slice.n - 1do
    f i (nth i slice)
    done
      
  let iter f slice =
    iteri (fun _ x -> f x) slice
      
  let to_vector slice =
    let vector = Poly.empty () in
    iter (fun v -> Poly.push v vector) slice;
    vector
end

include Poly
