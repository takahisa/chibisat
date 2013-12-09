{
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
open Parser
}

let digit   = ['0'-'9']
let newline = ['\r' '\n']
let space   = [' ' '\t']

let c = "c"
let p = "p" space+ "cnf" space+ digit+ space+ digit+

rule token = parse
   | space+                           { token lexbuf }
   | newline+                         { token lexbuf }
   | eof                              { EOF }
   | c                                { comment lexbuf; token lexbuf }
   | p                                { let lexeme = Lexing.lexeme lexbuf in
                                        Scanf.sscanf lexeme "p cnf %d %d"
                                          (fun nvars nclauses -> DIMACS(nvars, nclauses)) }
   | '0'                              { EOC }
   | '-'? ['1'-'9']['0'-'9']*         { let lexeme = Lexing.lexeme lexbuf in
                                        LIT(int_of_string lexeme) }
   | _                                { failwith (Printf.sprintf "unexpected token %s detected (%d-%d)"
                                                   (Lexing.lexeme lexbuf)
                                                   (Lexing.lexeme_start lexbuf)
                                                   (Lexing.lexeme_end lexbuf)) }
and comment = parse
   | eof                              { () }
   | newline                          { () }
   | _                                { comment lexbuf }
