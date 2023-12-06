(* ------------------------ *)
(*        tableaux          *)
(* ------------------------ *)


let arr_make(n, v : int * 'a) : 'a array = 
  if n < 0
  then failwith("erreur arr_make ; parametre invalide")
  else Array.make n v 
;;

let mat_make(n, m, v : int * int * 'a) : 'a array array = 
  if n < 0 || m < 0
  then failwith("erreur mat_make ; parametre invalide")
  else Array.make_matrix n m v 
;;