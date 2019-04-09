(* Question 1. *)

let rec common twolists = 
  let (l1, l2) = twolists in
  match l1 with
  | [] -> []
  | x :: xs ->
      if memberof(x, l2) then x::common(xs, remove(x, l2)) else common(xs, l2) 
;;


(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let rec split l =
  match l with
  | []
  | [_] as t1 -> t1, []
  | h::t ->
      let t1, t2 = split t in
      h::t2, t1
;;

(* Question 3 Here you implement merge. *)

let rec merge twolists =
  match twolists with
  | list, [] -> list
  | [], list -> list
  | h1::t1, h2::t2 ->
      if h1 <= h2 then
        h1 :: merge (t1, h2::t2)
      else
        h2 :: merge (h1::t1, t2);;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let rec mergesort l =
  match l with
  | [] -> []
  | [_] -> l
  | l ->
      let l1, l2 = split l in
      merge (mergesort l1, mergesort l2)
;;
