(*******************************************************************************
   Filename: practice.ml

     Author: David C. Drake (https://davidcdrake.com)

Description: A practice file for experimenting with the OCaml programming
             language.
*******************************************************************************)

let average a b =
  (a +. b) /. 2.0

prompt_string ("Name: ")

let rec union a = function
  | [] -> a
  | x::xs -> union (if member x a then a else x::a) xs;

let rec eval x = function
  | [] -> 0.0
  | c::cs -> c +. x *. eval x cs;
