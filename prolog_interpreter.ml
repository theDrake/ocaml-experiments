(******************************************************************************
   Filename: prolog_interpreter.ml

     Author: David C. Drake (http://davidcdrake.com), with initial assistance
             from Dr. Russ Ross (http://cit.dixie.edu/faculty/ross.php)

Description: A simple Prolog interpreter written in OCaml.
******************************************************************************)

open Core.Std

(******************************************************************************
  Helper functions
******************************************************************************)
let (++) f g x = f (g x)
let map        = List.map
let length     = List.length

(******************************************************************************
  A Prolog term can be an Atom, a Variable, or a Compound term. All lexemes are
  represented using strings.
******************************************************************************)
type term =
  Atom of string     |
  Variable of string |
  Compound of string * term list

(******************************************************************************
  Returns a string representation (pretty-printed) of a list of terms.
******************************************************************************)
let rec tl_to_string = function
    [] -> ""
  | [Atom(c)] -> c
  | [Variable(n)] -> n
  | [Compound(n, tl)] -> n ^ "(" ^ tl_to_string tl ^ ")"
  | (term::rest) -> tl_to_string [term] ^ "," ^ tl_to_string rest

(******************************************************************************
  Returns a string representation (pretty-printed) of a single term.
******************************************************************************)
let term_to_string t = tl_to_string [t]

(******************************************************************************
  Converts a term into a new term formed by applying the function f to all the
  variables (but leaving all atoms and predicate names unaltered). This can be
  used to apply a substitution to a term.
******************************************************************************)
let rec mapVariable f  = function
    (Atom x) -> Atom(x)
  | (Variable n) -> f n
  | (Compound(n, terms)) -> Compound(n, map ~f:(mapVariable f) terms)

(******************************************************************************
  Creates a substitution function that can be applied to a term. Takes a
  variable name and a term and returns a function that can be used to apply
  that substitution to any term.
******************************************************************************)
let sub name term =
  mapVariable (fun n -> if n=name then term else Variable n)

(******************************************************************************
  Find a most general unifier of two terms. Returns a pair of values, i.e.:

      let (unifiable, unifier) = mgu (a, b)

  The first value is a boolean, true iff the two terms are unifiable. If they
  are, the second value is a most general unifier: a function that, when
  applied to a and b, makes them identical. We do not perform the occurs check.
******************************************************************************)
let mgu (a, b) =
  let rec ut = function
          ([], [], unifier) ->
            (true, unifier)
        | (term::t1, Variable(name)::t2, unifier) ->
            let r = (sub name term) ++ unifier in
              ut (map ~f:r t1, map ~f:r t2, r)
        | (Variable(name)::t1, term::t2, unifier) ->
            let r = (sub name term) ++ unifier in
              ut (map ~f:r t1, map ~f:r t2, r)
        | (Atom(n)::t1, Atom(m)::t2, unifier) ->
             if n=m then ut (t1,t2,unifier) else (false, unifier)
        | (Compound(n1,xt1)::t1, Compound(n2,xt2)::t2, unifier) ->
             if n1=n2 && length xt1 = length xt2
             then ut (xt1@t1, xt2@t2, unifier)
             else (false, unifier)
        | (_,_,unifier) ->
             (false, unifier);
  in
    ut ([a],[b], (fun x -> x))

(******************************************************************************
  function resolution(clause, goals):
      let sub = the MGU of head(clause) and head(goals)
      return sub(tail(clause) concatenated with tail(goals))

  val resolution : term list -> term list -> term list = <fun>
******************************************************************************)
let resolution clause goals =
  let sub = mgu (head clause) (head goals)
  -> sub (tail clause) @ (tail goals)

(******************************************************************************
  function solve(goals)
      if goals is empty then succeed()
      else for each clause c in the program, in order
        if head(c) does not unify with head(goals) then do nothing
        else solve(resolution(c, goals))

  val solve : term list list -> term list -> bool list = <fun>
******************************************************************************)
let solve program goals =
  if goals=[] then -> [true]
  else

(******************************************************************************
  val prolog : term list list -> term -> bool list = <fun>
******************************************************************************)
let prolog program query = []

(******************************************************************************
  Challenge problem number one. A program with four clauses and a query. This
  one does not require variable renaming.
******************************************************************************)

(* 1. p(f(Y)) :- q(Y),r(Y). *)
let c1 = [Compound("p", [Compound("f", [Variable("Y")])]);
          Compound("q", [Variable("Y")]);
          Compound("r", [Variable("Y")])]

(* 2. q(g(Z)). *)
let c2 = [Compound("q", [Compound("g", [Variable("Z")])])]

(* 3. q(h(Z)). *)
let c3 = [Compound("q", [Compound("h", [Variable("Z")])])]

(* 4. r(h(a)). *)
let c4 = [Compound("r", [Compound("h", [Atom("a")])])]

(* The program. *)
let program = [c1; c2; c3; c4]

(* The query: p(X). *)
let query = Compound("p", [Variable("X")])

(* To test with this problem, call prolog program query. *)
