%{
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
%}

%token EOF
%token EOC
%token <int * int> DIMACS
%token <int>       LIT

%type <(int * int * Lit.t list list)> dimacs
%start dimacs
%%

dimacs:
| DIMACS conjunction
    { let (nvars, nclauses) = $1 in
      (nvars, nclauses, $2) }

conjunction:
| disjunction conjunction
    { $1 :: $2 }
| EOF
    { [] }

disjunction:
| LIT disjunction
    { if $1 >= 0 then
	Lit.make_t (abs $1 - 1) :: $2 
      else
        Lit.make_f (abs $1 - 1) :: $2 }
| EOC
    { [] }
