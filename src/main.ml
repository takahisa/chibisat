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

let print_model model =
  let open Printf in
  List.iteri (fun i sign ->
    printf "%d " (if sign then (i + 1) else -(i + 1))) model

let print_satisfiable model =
  let open Printf in
  printf "s SATISFIABLE\n";
  printf "v ";
  print_model model;
  printf "\n"

let print_unsatisfiable () =
  let open Printf in
  printf "s UNSATISFIABLE\n"

let solve assumps nvars nclauses clauses =
  let solver = Solver.make () in
  for i = 0 to nvars - 1 do
    ignore (Solver.new_var solver)
  done;
 for i = 0 to nclauses - 1 do
    ignore (Solver.new_clause (List.nth clauses i) solver)
  done;
  try
    Solver.solve [] solver
  with  
  | Solver.SAT model -> 
    print_satisfiable model;
    exit 10
  | Solver.UnSAT ->
    print_unsatisfiable ();
    exit 20

let gen_spec_list () =
  []

let gen_banner () = 
  "Chibisat Copyright (c) 2013 Takahisa Watanabe\n" ^
    (Printf.sprintf "--usage: %s  [options] input.cnf\n" Sys.argv.(0))

let () =
  let speclist = gen_spec_list () in
  let banner = gen_banner () in
  let filepath = ref "" in
  Arg.parse speclist (fun v -> filepath := v) banner;
  if !filepath = "" then
    begin Arg.usage speclist banner;
          exit 1 end;

  if not (Sys.file_exists !filepath) then
    begin Printf.eprintf "ERROR: file \"%s\" does not exists\n" !filepath;
          exit 1 end;
  let channel = open_in !filepath in
   
try
  let (nvars, nclauses, clauses) = Parser.dimacs Lexer.token (Lexing.from_channel channel) in
  ignore (solve [] nvars nclauses clauses);
  close_in channel
  with
    e -> close_in channel; raise e
