(*Exercices sur le match*)

(*1*)
let bool2int b = 
  match b with 
  |false -> 0
  |true -> 1 ;;

(*2*)
let int2bool i= 
  match i with 
  |0 -> false
  |1 -> true
  |_ -> failwith "erreur" ;;
(*les deux premiers cas donnent un filtre non-exhaustif*)
    
(*3*)
let int2romain c =
  match c with 
  |1 -> "I"
  |2 -> "II"
  |3 -> "III"
  |4 -> "VI"
  |5 -> "V"
  |6 -> "VI"
  |7 -> "VII"
  |8 -> "VIII"
  |9 -> "IX"
  |_ -> failwith "int2romain : chiffres seulement";; 

(*récursivité*)

(*1*)
let rec fact2 n = 
  match n with
  |0 -> 1
  |n -> n*(fact2(n-1));;

let rec fact1 n = if (n = 0) then 1 else (fact1(n-1)) * n;; 

(*2*)
let rec serie i = 
  match i with
  |0 -> 1.0
  |n -> (serie(i-1)) +. (1. /. (2. ** float_of_int n));;

let rec serie2 i = 
  match i with
  |0 -> 1.0
  |i -> 1.0 +. (serie(i-1)) /. 2.;;

(*3*)
let rec fibo n = 
  match n with 
  |0 -> 0 
  |1 -> 1
  |n -> fibo(n-1) + fibo(n-2);;
                         
let rec fibo_int n x1 x2 = 
  match n with 
  |0 -> x2
  |1 -> 1
  |n -> fibo_int (n-1) x2 (x2 + x1);;

(*4*)
let rec combi n p = 
  match (n,p) with 
  |(n,0) -> 1
  |(n,p) -> if(n = p) then 1 else combi (n-1) (p-1) + combi (n-1) p ;;

let rec c p n = 
  match p with
  |0 -> 1
  |p when p = n -> 1
  |p -> (c (p-1) (n-1) + (c p (n-1)));;

(*5*)
let rec acker m n =
  match (m,n) with 
  |(0,n) -> n+1
  |(m,0) -> acker (m-1) 1
  |(m,n) -> acker (m-1) (acker m (n-1));;

(*récursivité et listes*)
(*1*)
let rec after0 l =
  match l with
  |[] -> []
  |0::r -> r
  |_::r -> after0 r;;

(*2*)
let rec before0 l =
  match l with
  |[] -> [] 
  |0::_ -> []
  |a::r -> a::before0 r;;

(*3*)
let rec remove0 l = 
  match l with 
  |[] -> []
  |0::r -> remove0 r
  |a::r -> a::remove0 r;;

(*4*)
let rec add0 l =
  match l with 
  |[] -> []
  |[x] -> [x]
  |a::r -> a::0::add0 r;;

(*5*)
let rec opposite l = 
  match l with 
  |[] -> []
  |a::r -> (-a)::opposite r;;

(*6*)
(*let rec mem a l = 
match l with
|[] -> false
|b::_ when a = b -> true
|_::r -> mem a r;;*)

let rec mem l a = 
  match l with
  |[] -> false
  |n::r -> (n=a)||(mem r a) ;;

(*7*)
let rec memsort l a = 
  match l with 
  |[] -> false
  |b::_ when a = b -> true
  |b::_ when a > b -> false
  |_::r -> memsort r a;;

(*8*)
let rec nth l i =
  match l with 
  |[] -> failwith "liste vide"
  |x::r when i=1 -> x
  |x::r -> nth r (i-1);;

(*9*)
let rec append l1 l2 = 
  match l1 with
  |[] -> l2
  |x::r -> x::append r l2;;

(*Exercices supplémentaires*)

(*1*)
let rec flatten l = 
  match l with 
  |[] -> []
  |x::r -> append x (flatten r);;






















