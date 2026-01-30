(* Apprendre l'environnement d'Ocaml *)
let rec exp x n = if (n=0) then 1 else x*(exp x (n-1));;

let rec exp2 x n = 
  match n with 
  |0 -> 1
  |n -> x*(exp x (n-1));;

(* Programmation des suites *)

let rec u n = match n with
  |0 -> 1
  |n -> n+1 ;;

let rec suite2 x = 
  match x with 
  |0 -> 2
  |1 -> 3
  |x -> suite2 (x-1) + suite2(x-2);;

let rec suite3 x =
  match x with 
  |0 -> 2
  |1 -> 3
  |x -> suite3 (x-1) * suite3(x-2);;

let rec suite4 x =
  match x with 
  |0 -> 2
  |x -> suite4(x-1) + 4;;

let rec suite5 x = 
  match x with 
  |0 -> 0
  |1 -> 1
  |x -> suite5(x-1) + suite4(x-2);;

(* Fonctions simples *)

(* Exercice 1 *)

let mon_poly x = x*x+2*x+1;;
mon_poly 3;;

let mon_poly x = x*x-2*x+1;;
mon_poly 3;;

(* Exercice 2 *)

let pi = 3.14;;
let volume_sphere r = (4.0/.3.0) *. pi *. (r *. r *. r);;

let surface_disque r = pi *. (r ** 2.0);;

let surface_rectangle longeur largeur = longeur *. largeur;;

let surface_triangle b h = (b *. h) /. 2.0;;

(* Exercice 3 *)

let pair x = if ((x mod 2) == 0) then true else false;;

(* Exercice 4 *)

let triangle l1 l2 l3  = if (l1 + l2 >= l3) && (l2 + l3 >= l1) && (l3 + l1 >= l2) then true else false;;

let triangle_rectangle a b c = if (triangle a b c) && ((a*a + b*b = c*c) || (a*a + c*c = b*b) || (c*c + b*b = a*a)) then true else false;;

(* If...then...else... *)

(* Exercice 5 *)

let max x1 x2 = if (x1 > x2) then x1 else x2;;

(* Exercice 6 *)

let max3 x1 x2 x3 = if ((x1>x2) && (x1>x3)) then x1 else if (x2>x1) && (x2>x3) then x2 else x3;;
let max3_2 x1 x2 x3 = max (x1 (max x2 x3))

(* Exercice 7 *)

let abs x = if (x>0) then x else -(x);;

(* Exercice 8 *)

let infini = 1.0 /. 0.0;;

let discr a b c = if ((b*.b) -. 4.0*.a*.c) >= 0.0 then ((b*.b) -. 4.0*.a*.c) else infini;;

let sol1 a b c = ((-.b) +. sqrt(discr a b c)) /. 2.0 *. a;;

let sol2 a b c = ((-.b) -. sqrt(discr a b c)) /. 2.0 *. a;;
  
    












