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
open OUnit
open Solver
module C = Constr

let (?+) varid = Lit.make_t varid
let (?-) varid = Lit.make_f varid

let constr_is_tautology_test =
  "constr_is_tautology" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      begin let target = C.of_list [(?+v1); (?-v1); (?-v2); (?-v3)] in
        assert_equal true (constr_is_tautology target) end
  end

let constr_is_satisfied_test =
  "constr_is_satisfied" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      let constr = Constr.of_list [?-v1; ?-v2; ?-v3] in
      begin let target = constr in
        assert_equal false (constr_is_satisfied target solver) end;
      begin assume (Lit.make_f v1) solver;
        let target = constr in
        assert_equal true (constr_is_satisfied target solver) end
  end

let constr_simplify_test =
  "constr_simplify" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      begin let target = C.of_list [?-v1; ?-v1; ?-v2; ?-v3] in
            let expect = C.of_list [?-v1; ?-v2; ?-v3] in
            constr_simplify target solver;
            assert_equal true (C.sequential_equal expect target) end;
      begin assume (?+v1) solver;
            let target = C.of_list [?-v1; ?-v2; ?-v3] in
            let expect = C.of_list [?-v2; ?-v3] in
            constr_simplify target solver;
            assert_equal true (C.sequential_equal expect target) end
  end

let find_level_highest_test =
  "find_level_highest" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      let c = C.of_list [(?+v1); (?+v2); (?-v3)] in
      assume (?+v1) solver;
      assume (?+v2) solver;
      assume (?-v3) solver;
      let expect = 2 in
      assert_equal expect (find_level_highest c solver)
  end

let prop_enqueue_test =
  "prop_enqueue" >:: begin
    fun () ->
      let solver = make () in
      let v = Var.id (new_var solver) in
      let d = decision_level solver in
      prop_enqueue (?+v) solver;
      assert_equal Tribool.T (lit_value (?+v) solver);
      assert_equal d (decision_level solver)
  end

let prop_enqueue_with_test =
  "prop_enqueue_with" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let c = C.of_list [?+v1; ?+v2] in
      assume (?-v2) solver;
      let d = decision_level solver in
      prop_enqueue_with (?+v1) c solver;
      assert_equal Tribool.T (lit_value (?+v1) solver);
      assert_equal d (decision_level solver);
      assert_equal true (C.sequential_equal c (Option.get (reason (?+v1) solver)))
  end

let assume_test =
  "assume" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      prop_enqueue (?+v1) solver;
      prop_enqueue (?+v2) solver;
      let d = decision_level solver in
      assume (?+v3) solver;
      assert_equal (d + 1) (decision_level solver)
  end


let undo_test =
  "undo" >:: begin
    fun () ->
      let solver = make () in
      let lit = ?+(Var.id (new_var solver)) in
      assume lit solver;
      undo solver;
      assert_equal Tribool.Undef (lit_value lit solver);
      assert_equal (-1) (lit_level lit solver)
  end

let cancel_test =
  "cancel" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      let v4 = Var.id (new_var solver) in
      assume (?+v1) solver;
      let d = decision_level solver in
      assume (?+v2) solver;
      prop_enqueue (?+v3) solver;
      prop_enqueue (?+v4) solver;
      cancel solver;
      assert_equal ~printer:string_of_int (-1) (lit_level (?+v2) solver);
      assert_equal ~printer:string_of_int (-1) (lit_level (?+v3) solver);
      assert_equal ~printer:string_of_int (-1) (lit_level (?+v4) solver);
      assert_equal d (decision_level solver)
  end

let cancel_until_test =
  "cancel_until" >:: begin
    fun () ->
      let solver = make () in
     let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      let v4 = Var.id (new_var solver) in
      let v5 = Var.id (new_var solver) in
      assume (?+v1) solver;
      let d = decision_level solver in
      assume (?+v2) solver;
      assume (?+v3) solver;
      assume (?+v4) solver;
      assume (?+v5) solver;
      cancel_until d solver;
      assert_equal d (decision_level solver);
  end

let propagate_test =
  "propagate" >:: begin
    fun () ->
      let solver = make () in
      let v1 = Var.id (new_var solver) in
      let v2 = Var.id (new_var solver) in
      let v3 = Var.id (new_var solver) in
      let lits = [?+v1; ?+v2; ?+v3] in
      ignore (new_clause lits solver);
      assume (?-v2) solver;
      assume (?-v3) solver;
      propagate solver;
      assert_equal Tribool.T (lit_value (?+v1) solver)
  end

let _ =
  run_test_tt_main begin
    "Solver" >::: [
      constr_is_tautology_test;
      constr_is_satisfied_test;
      constr_simplify_test;
      find_level_highest_test;
      prop_enqueue_test;
      prop_enqueue_with_test;
      assume_test;
      undo_test;
      cancel_test;
      cancel_until_test;
      propagate_test
    ]
  end
