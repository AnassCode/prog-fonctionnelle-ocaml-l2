(*let a = 3. ;;
 let b = 3. ;;

 a = b ;; 
 a == b ;;
 a != b ;;
 a <> b ;;

 let c = 3 ;;
 let d = 3 ;;

 c = d;;
 c == d ;;
 c != d ;;
 c <> d ;;*)

(*1*)
let rec mem c  r = 
  match r with 
  | a::r when a=c -> true 
  | a::r when a < c -> mem c r 
  | _ -> false ;;

(*2*)
let rec existing x rest = 
  match rest with 
  |[] -> false 
  |(_,y)::_ when x=y -> true
  |_::rest -> existing x rest;;

let rec img r =
  match r with 
  |[] -> []
  |(x,y)::rest when existing y rest -> img rest 
  |(x,y)::rest -> y::(img rest);;

let rec existant x r =
  match r with
  | [] -> false
  | (y,_)::s -> (x = y) || existant x s ;;

let rec antecedent r = 
  match r with 
  |[] -> [] 
  |(x,_)::s when existant x s -> antecedent s
  |(x,_)::s -> (antecedent s);;

let rec antecedent r2 = 
  match r2 with 
  |[] -> []
  |[(x,y)] -> [x]
  |(x1,y1)::(x2,y2)::rest when x1 = x2 -> antecedent ((x2,y2)::rest);;

(*3*)
let rec is_fun r =
  match r with 
  |[] -> true
  |(x,_)::s -> (not (existant x s)) && is_fun s;;

(*let rec is_fun2 r = 
   match r with 
   |[] -> true 
   |_::[] -> true 
   |(x1,_)::(x2,y2)::s -> x1 <> x2 && (is_fun (x2,y2)::s);;*)

(*4*)
let rec union l1 l2 = 
  match l1,l2 with 
  |([],l2) -> l2
  |(l1,[]) -> l1
  |(a1::r1,a2::r2) when a1<a2 -> a1::(union r1(a2::r2))
  |(a1::r1,a2::r2) when a1>a2 -> a2::(union(a1::r1)r2)
  |(a1::r1,_::r2) -> a1::(union r1 r2);;

(*5*)
let rec intersection l1 l2 =
  match (l1,l2) with
  |([],l2) -> []
  |(l1,[]) -> []
  |(a1::r1, a2::r2) when a1 < a2 -> intersection r1 l2
  |(a1::r1, a2::r2) when a1 > a2 -> intersection l1 r2
  |(a1::r1, _::r2)  -> a1::(intersection r1 r2);;

(*6*)
let rec length l = 
  match l with 
  |[] -> 0
  |_::r -> 1 + (length r);;

let rec before x l = 
  match (x,l) with 
  |(0,_) -> []
  |(_,[]) -> [] (*ou failwith dans le cas du tri fusion*)
  |(x,a::r) -> a::before(x-1) r;;

let rec after x l =
  match (x,l) with
  |(0,r) -> r 
  |(_,[]) -> [] (*ou failwith dans le cas du tri fusion*) 
  |(x,_::r)-> after(x-1) r;;

let split l = (before((length l)/2) l, after ((length l)/2) l) ;;

let rec split2 l =
  match l with 
  |[] -> ([],[])
  |[a] -> ([a],[]) 
  |a::b::r -> let (l1,l2) = split2 r in (a::l1,b::l2);;

let rec sortset l = 
  match l with 
  |[] -> []
  |[a] -> [a]
  |l -> let (l1,l2) = split l in union (sortset l1)(sortset l2);;

(*7*)
let rec invert1 l = 
  match l with 
  |[] -> []
  |(x,y)::r -> (y,x)::invert1 r ;;

let rec invert l = sortset(invert l);;

(*8*)
let rec linkit x l =
  match l with 
  |[] -> []
  |a::r -> (x,a)::linkit x r;;

(*9*)
let rec img x r = 
  match r with
  |[] -> []
  |(a,b)::l1 when a = x -> b ::img x l1
  |_::l1 -> img x l1 ;;
  
let rec composition  l1 l2 =
  match l1 with
  |[]->[]
  |(a,b)::r -> union (composition r l2) (linkit a (img b l2)) ;; 

(*10*)
let rec fusion r = 
  match r with 
  |[] -> []
  |(x,y)::reste -> (x,x)::(y,y)::fusion reste;;

(*let rec exist x r =    
   match r with 
   |[] -> false 
   |a::r when a = x -> true 
   |a::r -> exist x r;;
    
 let rec sans_doublon r = 
   match r with 
   |[] -> []
   |x::r when exist x r -> sans_doublon r
   |x::r -> x::sans_doublon r;;*)

let rec identity r = sortset(fusion r);;
  
let rec img3 x r =
  match r with
  | [] -> false
  | (a,b)::rest when a = x -> true
  | _::rest -> img3 x rest;;

let rec img4 x r =
  match r with
  | [] -> failwith "error"
  | (a,b)::rest when a = x -> b
  | _::rest -> img3 x rest;;

let rec closure r = 
  match r with
  |(x1,y1) :: reste when img3 y1 r -> (x1,img4 y1 r)::closure reste
  |(x1,y1) :: reste when not(img3 y1 r) -> closure reste
;;
  
(*11*)
let rec img2 x r =
  match r with 
  |[]->false
  |(x1,y)::r when x1=x -> true
  |_::rest -> img2 x rest

let rec reachable r x y = 
  match r with 
  |[] -> false
  |(x1,y1)::rest when x1=x && y1=y -> true
  |(x1,y1)::rest when img2 y1 rest -> reachable rest y1 y
  |(x1,y1)::rest -> false;; 


    
    
    
    
    
    
    