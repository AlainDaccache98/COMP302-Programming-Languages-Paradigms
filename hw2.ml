(* Helper *)
let rec sumlist l =
  match l with
  | [] -> 0.0
  | x :: xs -> x +. sumlist xs
;;

(* Question 1. *)

let rec pairlists (l1, l2) =
  match l1 with 
  | [] -> [] 
  | x::xs -> match l2 with
    | [] -> []
    | y:: ys -> (x , y)::(pairlists(xs, ys)) 
      
;;

let wmean weights data =
  let l = pairlists(weights, data) in
  let mad = List.map(fun (n, m) -> n*.m) l in 
  sumlist mad /. sumlist weights

(* Question 2. *)

let rec memberof (n, l) =
  match l with
  | [] -> false
  | x::xs -> 
      if x = n then true
      else memberof(n, xs) 
;;

let rec remove (item, lst) =
  match lst with 
  | []  -> lst
  | x::t -> if x = item then remove(item, t) else x::remove(item, t) 
;;

(* Question 3. *)

let find_max l =
  let rec helper a l = match l with
    | [] -> a
    | h :: t -> helper (max a h) t
  in helper (-99999999) l
;;

(* Question 4. *)

let rec selsort l =
  let max = find_max l in
  let newlist = remove(max, l) in
  match l with 
  |[] -> []
  | _ -> max::selsort newlist
;;
