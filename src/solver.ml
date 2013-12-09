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

exception SAT of bool list
exception UnSAT
exception Conflict of Constr.t

module V = Vector
module S = Vector.Slice
module Q = Queue
module M = HashMap

type t = {
  mutable watches    : Constr.t Vector.t Vector.t;
  mutable constrs    : Constr.t Vector.t;
  mutable learnts    : Clause.t Vector.t;
  mutable reason     : (VarId.t, Constr.t) HashMap.t;
  mutable vars       : Var.t Vector.t;
  mutable assigns    : Tribool.t Vector.t;
  mutable props      : Lit.t Queue.t;
  mutable trail      : Lit.t Vector.t;
  mutable trail_lim  : int Vector.t;
  mutable level      : int Vector.t;
  mutable root_level : int;
}

let make () = {
  watches    = V.empty ();
  constrs    = V.empty ();
  learnts    = V.empty ();
  reason     = M.empty ();
  vars       = V.empty ();
  assigns    = V.empty ();
  props      = Q.create ();
  trail      = V.empty ();
  trail_lim  = V.empty ();
  level      = V.empty ();
  root_level = 0
}

let reset solver = 
  V.clear solver.watches;
  V.clear solver.constrs;
  V.clear solver.learnts;
  M.clear solver.reason;
  V.clear solver.vars;
  V.clear solver.assigns;
  Q.clear solver.props;
  V.clear solver.trail;
  V.clear solver.trail_lim;
  V.clear solver.level;
  solver.root_level <- 0

let decision_level solver =
  V.size solver.trail_lim

let lit_index lit =
  let open Lit in
  match lit.sign with
  | Sign.T -> lit.id lsl 1 + 1
  | Sign.F -> lit.id lsl 1

let lit_level lit solver =
  V.nth (Lit.id lit) solver.level

let lit_value lit solver =
  let open Lit in
  match (lit.sign, V.nth lit.id solver.assigns) with
  | Sign.T, Tribool.T -> Tribool.T
  | Sign.F, Tribool.F -> Tribool.T
  | Sign.T, Tribool.F -> Tribool.F
  | Sign.F, Tribool.T -> Tribool.F
  | _, Tribool.Undef  -> Tribool.Undef

let nvars solver =
  V.size solver.vars

let nclauses solver =
  V.size solver.constrs

let nassigns solver =
  V.size solver.trail

let model_found solver =
  nassigns solver = nvars solver

let model solver =
  V.fold_right (fun v xs -> (Tribool.to_bool v)::xs) solver.assigns []

let watch_list lit solver =
  V.nth (lit_index lit) solver.watches

let prop_enqueue lit solver =
  match lit_value lit solver with
  | Tribool.T -> ()
  | Tribool.F -> raise UnSAT
  | _ ->
    let index = Lit.id lit in
    let value = Lit.Sign.to_tribool (Lit.sign lit) in
    begin V.update index value solver.assigns;
          V.update index (decision_level solver) solver.level;
          Q.push lit solver.props;
          V.push lit solver.trail end

let prop_enqueue_with lit reason solver =
  match lit_value lit solver with
  | Tribool.T -> ()
  | Tribool.F -> raise (Conflict reason)
  | _ ->
    let index = Lit.id lit in
    let value = Lit.Sign.to_tribool (Lit.sign lit) in
    begin V.update index value solver.assigns;
          V.update index (decision_level solver) solver.level;
          Q.push lit solver.props;
          V.push lit solver.trail;
          M.add index reason solver.reason end

let constr_is_tautology constr =
  let open Constr in
  let rec loop i n =
    if i >= n then
      false
    else
      let elt = nth i constr in
      let slice = S.make (i + 1) (n - i - 1) constr in
      match S.find ((=) (Lit.neg elt)) slice with
      | Some _ -> true
      | None   -> loop (i + 1) n
  in
  loop 0 (size constr)

let constr_is_satisfied constr solver =
  let open Constr in
  exist (fun lit -> lit_value lit solver = Tribool.T) constr || constr_is_tautology constr

let constr_simplify constr solver =
  let open Constr in
  List.iter (fun elt -> remove elt constr) 
    (List.filter (fun lit -> lit_value lit solver = Tribool.F) (to_list constr));
  let rec loop i n =
    if i < n then
      let elt = nth i constr in
      let slice = S.make (i + 1) (n - i - 1) constr in
      begin match S.find ((=) elt) slice with
            | None   -> loop (i + 1) n
            | Some i -> 
              begin remove_at i constr;
                    loop i (n - 1) end end;
  in
  loop 0 (size constr)

let constr_propagate prop constr solver =
  let open Constr in
  if head constr = Lit.neg prop then
    begin update 0 (nth 1 constr) constr;
          update 1 (Lit.neg prop) constr end;
  
  if lit_value (head constr) solver = Tribool.T then
    V.push constr (watch_list prop solver)
  else
    let rec loop i n =
      if i >= n then
        begin V.push constr (watch_list prop solver);
              prop_enqueue_with (nth 0 constr) constr solver end
      else if lit_value (nth i constr) solver = Tribool.F then
        loop (i + 1) n
      else
        begin update 1 (nth i constr) constr;
              update i (Lit.neg prop) constr;
              V.push constr (watch_list (Lit.neg (nth 1 constr)) solver) end
    in
    loop 2 (size constr)

let find_level_highest constr solver =
  let open Constr in
  if size constr = 0 then
    raise (Invalid_argument "constr");
  let rec loop i j n level_max =
    if i >= n then
      j
    else
      let lit = nth i constr in
      let level = lit_level lit solver in
      if level > level_max then 
        loop (i + 1) i n level
      else 
        loop (i + 1) j n level_max 
  in
  loop 0 (-1) (size constr) (-1)

let reason lit solver =
  M.find (Lit.id lit) solver.reason


let assume lit solver =
  V.push (V.size solver.trail) solver.trail_lim;
  prop_enqueue lit solver

let undo solver =
  let lit = V.pop solver.trail in
  let index = Lit.id lit in
  V.update index Tribool.Undef solver.assigns;
  V.update index (-1) solver.level;
  if M.exist index solver.reason then
    M.erase index solver.reason

let cancel solver =
  let n = V.size solver.trail - V.pop solver.trail_lim in
  for i = 0 to n - 1 do
    undo solver
  done

let cancel_until level solver =
  while decision_level solver > level do
    cancel solver
  done

let new_var solver =
  let v = Var.make (nvars solver) in
  begin V.push (V.empty ()) solver.watches;
    V.push (V.empty ()) solver.watches;
    V.push v solver.vars;
    V.push Tribool.Undef solver.assigns;
    V.push (-1) solver.level;
    v end

let new_learnt lits solver =
  let open Constr in
  let constr = of_list lits in
  let clause = Clause.make constr in
  match size constr with
  | 0 -> raise UnSAT
  | 1 ->
    begin prop_enqueue (nth 0 constr) solver;
          clause end
  | _ ->
    let i = find_level_highest constr solver in
    begin update 1 (List.nth lits i) constr;
          update i (List.nth lits 1) constr;
          V.push constr (watch_list (Lit.neg (nth 0 constr)) solver);
          V.push constr (watch_list (Lit.neg (nth 1 constr)) solver);
          V.push constr solver.constrs;
          V.push clause solver.learnts;
          prop_enqueue_with (nth 0 constr) constr solver;
          clause end

let new_clause lits solver =
  let open Constr in
  let constr = of_list lits in
  let clause = Clause.make constr in
  if constr_is_satisfied constr solver then
    clause
  else
    begin constr_simplify constr solver;
          match size constr with
          | 0 -> raise UnSAT
          | 1 -> 
            begin prop_enqueue (nth 0 constr) solver;
                  clause end
          | _ ->
            begin V.push constr (watch_list (Lit.neg (nth 0 constr)) solver);
                  V.push constr (watch_list (Lit.neg (nth 1 constr)) solver);
                  V.push constr solver.constrs;
                  clause end end

let propagate solver =
  while not (Q.is_empty solver.props) do
    let prop = Q.pop solver.props in
    let watching = watch_list prop solver in
    let tmp = V.copy watching in
    V.clear watching;
    for i = 0 to V.size tmp - 1 do
      try constr_propagate prop (V.nth i tmp) solver
      with e ->
        for j = i + 1 to V.size tmp -1 do
          V.push (V.nth j tmp) watching
        done;
        raise e
    done
  done


let rec select_var solver =
  let rec loop i n =
    if i >= n then
      assert false (* should not reached here *)
    else if V.nth i solver.assigns = Tribool.Undef then
      i
    else
      loop (i + 1) n
  in
  loop 0 (V.size solver.assigns)

let analyze_conflict confl solver =
  let open Constr in
  let seen = V.make (nvars solver) false in
  let learnt = ref [] in
  let level  = ref 0 in
  let counter = ref 0 in
  let rec loop i r =
    for i = i to size r - 1 do
      let p = nth i r in
      let d = lit_level p solver in
      if not (V.nth (Lit.id p) seen) && d > 0 then
        begin V.update (Lit.id p) true seen;
              if d = decision_level solver then
                counter := !counter + 1
              else
                begin learnt := p :: !learnt;
                      level  := max d !level end end
    done;
    let p =
      let rec loop i =
        let lit = V.nth i solver.trail in
        if V.nth (Lit.id lit) seen then V.nth i solver.trail else loop (i - 1)
      in 
      loop (V.size solver.trail - 1) in
    let r = reason p solver in
    V.update (Lit.id p) false seen;
    counter := !counter - 1;
    if !counter > 0 then
      loop 1 (Option.get r)
    else
      begin learnt := Lit.neg p :: !learnt;
            (!learnt, !level) end
  in
  loop 0 confl

let search solver =
  while true do
    try
      propagate solver;

      if model_found solver then
        raise (SAT (model solver));

      let lit = Lit.make_t (select_var solver) in
      assume lit solver;
    with
      Conflict k ->
        if decision_level solver = solver.root_level then
          raise UnSAT;
        let (lits, d) = analyze_conflict k solver in
        cancel_until (max d solver.root_level) solver;
        ignore (new_learnt lits solver)
  done

let solve assumps solver =
  try
    List.iter (fun lit -> assume lit solver; propagate solver) assumps;
    solver.root_level <- decision_level solver;
    search solver
  with
    Conflict _ -> raise UnSAT
