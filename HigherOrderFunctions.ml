(* Helper Functions *)

let rec fact x =
  if x <= 1 then 1 
  else x * fact (x - 1);; 

let square x = x * x;;

let derivative dx f = fun x -> (f (x +. dx) -. f x) /. dx;;

(* Higher Order Functions *)

let apply_twice f x = f (f x);;  
apply_twice (function x -> 2*x) 1;;  

let rec apply_n_times f n x =   
  if n<=0 then x  
  else apply_n_times f (n-1) (f x);;  
apply_n_times (function x -> 2*x) 10 1;;  

(* Function Composition *)
let compose f g = (function x -> f (g x));;  
let fg = compose (function x->x+1) (function x->2*x) in  
fg 10;;   
let square_o_fact = compose square fact;;
square_o_fact 5;;

(* Returns the sum of the results of applying a given function f to each 
   element of a list *)
let rec sigma f = function
  | [] -> 0
  | x :: l -> f x + sigma f l;;
sigma (fun x -> x * x) [1; 2; 3];;

(* Takes a function f and a non-negative integer n as arguments and returns 
the function that applies f, n times *)
let rec repeated (f,n) = 
  if (n = 0) then fun x -> x 
  else fun x -> f ((repeated (f,n-1)) x);;

(* I think they're equivalent *)
let rec power f n = 
  if n = 0 then fun x -> x 
  else compose f (power f (n - 1));;
