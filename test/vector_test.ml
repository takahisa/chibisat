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
open Vector

let empty_test =
  "empty" >:: begin
    fun () ->
      let target = empty () in
      assert_equal 0 (size target);
  end

let make_test =
  "make" >:: begin
    fun () ->
      let n = 256 in
      let init = 42 in
      let target = make n init in
      assert_equal n (size target);
      for i = 0 to n - 1 do
    assert_equal init (nth i target)
      done
  end 

let size_test =
  "size" >:: begin
    fun () ->
      let n = 256 in
      let init = 42 in
      let target = make n init in
      assert_equal n (size target)
  end

let copy_test =
  "copy" >:: begin
    fun () ->
      let n = 256 in
      let init = 42 in
      let original = make n init in 
      let target = copy original in
      assert_equal (size original) (size target);
      for i = 0 to n - 1 do
    assert_equal (nth i original) (nth i target)
      done;
      update 0 (init + 1) original;
      assert_equal true (target <> original)
  end

let clear_test =
  "clear" >:: begin
    fun () ->
      let n = 256 in
      let init = 42 in
      let target = make n init in
      clear target;
      assert_equal 0 (size target)
  end

let of_list_test =
  "of_list" >:: begin
    fun () ->
      let source = [1; 2; 3] in
      let target = of_list source in
      assert_equal (List.length source) (size target);
      for i = 0 to size target - 1 do
    assert_equal (List.nth source i) (nth i target)
      done
  end

let to_list_test =
  "to_list" >:: begin
    fun () ->
      let expect = [1; 2; 3] in
      let source = of_list expect in
      assert_equal expect (to_list source)
  end

let nth_test =
  "nth" >:: begin
    fun () ->
      let expect = [1; 2; 3] in
      let target = of_list expect in
      for i = 0 to List.length expect - 1 do
    assert_equal (List.nth expect i) (nth i target)
      done
  end

let update_test =
  "update" >:: begin
    fun () ->
      let expect = 42 in
      let target = of_list [1; 2; 3] in
      update 0 expect target;
      assert_equal expect (nth 0 target)
  end

let head_test =
  "head" >:: begin
    fun () ->
      let expect = 42 in
      let dummy = 0 in
      let target = of_list [expect; dummy; dummy] in
      assert_equal expect (head target);
      assert_equal expect (head target)
  end

let last_test =
  "head" >:: begin
    fun () ->
      let expect = 42 in
      let dummy = 0 in
      let target = of_list [dummy; dummy; expect] in
      assert_equal expect (last target);
      assert_equal expect (last target)
  end

let push_test =
  "push" >:: begin
    fun () ->
      let value = 42 in
      let target = empty () in
      push value target;
      assert_equal 1 (size target);
      assert_equal value (head target)
  end

let pop_test =
  "pop" >:: begin
    fun () ->
      let expect = [1; 2; 3] in
      let target = of_list expect in
      assert_equal (List.nth expect 2) (pop target);
      assert_equal (List.nth expect 1) (pop target);
      assert_equal (List.nth expect 0) (pop target)
  end

let find_test =
  "find" >:: begin
    fun () ->
      let expect = 42 in
      let dummy = 0 in
      let target = of_list [dummy; expect; dummy] in
      match find ((=) expect) target with
      | Some i -> assert_equal 1 i
      | None   -> assert_failure "should not be reached here"
  end

let exist_test =
  "exist" >:: begin
    fun () ->
      let expect = 42 in
      let dummy = 0 in
      let target = of_list [dummy; expect; dummy] in
      assert_equal true (exist ((=) expect) target)
  end

let iteri_test =
  "iteri" >:: begin
    fun () ->
      let expect = 0 + 1 + 2 in
      let target = of_list [1; 2; 3] in
      let sum = ref 0 in
      iteri (fun i _ -> sum := !sum + i) target;
      assert_equal expect !sum
  end

let iter_test =
  "iter" >:: begin
    fun () ->
      let expect = 1 + 2 + 3 in
      let target = of_list [1; 2; 3] in
      let sum = ref 0 in
      iter (fun v -> sum := !sum + v) target;
      assert_equal expect !sum
  end

let remove_at_test =
  "remove_at" >:: begin
    fun () ->
      let expect = [1; 3] in
      let target = of_list [1; 2; 3] in
      remove_at 1 target;
      assert_equal expect (to_list target)
  end

let remove_test =
  "remove" >:: begin
    fun () ->
      let expect = [1; 3] in
      let target = of_list [1; 2; 3] in
      remove 2 target;
      assert_equal expect (to_list target)
  end

let fold_left_test =
  "fold_left" >:: begin
    fun () ->
      let expect = (((((0 - 1) - 2) - 3) - 4) - 5) in
      let target = of_list [1; 2; 3; 4; 5] in
      assert_equal expect (fold_left (fun x y -> x - y) target 0)
  end

let fold_right_test =
  "fold_left" >:: begin
    fun () ->
      let expect = (1 - (2 - (3 - (4 - (5 - 0))))) in
      let target = of_list [1; 2; 3; 4; 5] in
      assert_equal expect (fold_right (fun x y -> x - y)  target 0)
  end

let sequential_equal_test =
  "sequential_test" >:: begin
    fun () ->
      let target = of_list [1; 2; 3] in
      let expect = of_list [1; 2; 3] in
      assert_equal true (sequential_equal expect target)
  end

let _ = 
  run_test_tt_main begin
    "Vector" >:::[
      empty_test;
      make_test;
      size_test;
      copy_test;
      clear_test;
      of_list_test;
      to_list_test;
      nth_test;
      update_test;
      head_test;
      last_test;
      push_test;
      pop_test;
      find_test;
      exist_test;
      iteri_test;
      iter_test;
      remove_at_test;
      remove_test;
      fold_left_test;
      fold_right_test;
      sequential_equal_test
    ]
end
