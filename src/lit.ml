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

module Sign = struct
  type t = T | F

  let neg sign =
    match sign with
    | T -> F
    | F -> T

  let of_bool v =
    match v with
    | true  -> T
    | false -> F

  let to_bool sign =
    match sign with
    | T -> true
    | F -> false

  let to_tribool sign =
    match sign with
    | T -> Tribool.T
    | F -> Tribool.F
end

type t = {
  sign : Sign.t;
  id   : VarId.t
}

let make sign varid =
  { sign  = sign;
    id    = varid }

let make_t = make Sign.T
let make_f = make Sign.F

let sign lit =
  lit.sign

let id lit =
  lit.id

let neg lit =
  { lit with sign = Sign.neg lit.sign }

let show lit =
  match lit.sign with
  | Sign.T -> Printf.sprintf "T.%d" (lit.id + 1)
  | Sign.F -> Printf.sprintf "F.%d" (lit.id + 1)
