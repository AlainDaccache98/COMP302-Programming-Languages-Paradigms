let mysqrt (x:float) =
  let rec aux a = 
    let result = ((x /. a +. a) /. 2.0)
    in
    if close (x , square(result)) then result
    else aux result 
  in 
  aux 0.0001
;;
  
let cube_root (x:float) = 
  let rec aux a = 
    let result = (((a *. 2.0) +. (x /. square(a))) /. 3.0)
    in
    if close (x , cube(result)) then result
    else aux result 
  in 
  aux 0.00001
;;

let fast_exp (base, power) =
  let rec aux (acc, base, power) =
    if power = 0 then acc
    else if power = 1 then (acc*base) 
    else if odd power then aux ((acc * base), (base*base), ((power-1)/2))
    else aux (acc, (base*base), (power/2))
  in
  aux (1, base, power)
;;
