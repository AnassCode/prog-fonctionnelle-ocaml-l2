(*1*)

type adn_t = A | T | G | C ;;

let aatgcct = [A;A;T;G;C;C;T];;

(*2*)

type adndico_t = Node of adndico_t * adndico_t * adndico_t * adndico_t | Null of string;;    
let dico_vide = Null "" ;;

(*3*)

let rec height d = 
  match d with 
  |Null _ -> 0
  |Node(a,t,g,c) -> 1 + (max (max (height a) (height t)) (max(height g) (height c)));; 

(*4*)
let rec dicreate seq data = 
  let nodic = Null "" in 
  match seq with 
  |[] -> Null data
  |A::r -> Node(dicreate r data, nodic, nodic, nodic) 
  |T::r -> Node(nodic, dicreate r data, nodic, nodic) 
  |G::r -> Node(nodic, nodic, dicreate r data, nodic) 
  |C::r -> Node(nodic, nodic, nodic, dicreate r data) ;;

(*5*)
let rec insert seq dico data = 
  match (seq,dico) with 
  |([], Null _) -> Null data
  |([], Node _) -> failwith "sub_sequence insertion is not allowed."
  |(A::r, Node (a,t,g,c)) -> Node (insert r a data,t,g,c)    
  |(T::r, Node (a,t,g,c)) -> Node (a,insert r t data,g,c)   
  |(G::r, Node (a,t,g,c)) -> Node (a,t,insert r g data,c)   
  |(C::r, Node (a,t,g,c)) -> Node (a,t,g,insert r c data)
  |(_,Null _) -> dicreate seq data;;   

(*6*)
(* Exemple de la figure *)
let s1=[A;T;G;T;A] and
  s2=[A;T;C;A] and
  s3=[T;A;G;T] and
  s4=[G;A;A;A;C] and
  s5=[G;A;A;T;G] and
  s6=[T;C;G] and
  s7=[A;T;G;C];;

(* dseqs est le dictionnaire contenant les séquences de l'exemple *)
let dseqs =
  let d = (Null "") in
  let d = insert s1 d "s1" in
  let d = insert s2 d "s2" in
  let d = insert s3 d "s3" in
  let d = insert s4 d "s4" in
  let d = insert s5 d "s5" in
  let d = insert s6 d "s6" in
  let d = insert s7 d "s7" in d;;

(*7*)
let rec count dico = 
  match dico with 
  |Null "" -> 0
  |Null _ -> 1
  |Node (a,t,g,c) -> count a + count t + count g + count c;;

(*8*)
let rec search seq dico =
  match (seq, dico) with
  |([], Null data) -> data 
  |([], Node (_,_,_,_)) -> ""
  |(_, Null _) -> ""
  |(A::r, Node (a,t,g,c)) -> search r a 
  |(T::r, Node (a,t,g,c)) -> search r t
  |(G::r, Node (a,t,g,c)) -> search r g 
  |(C::r, Node (a,t,g,c)) -> search r c ;; 

(*9*)
(*la hauteur de l'arbre (le sequence la plus longue) : O(h) *)

(*10*)

let rec collectdata d =
  match d with
  |Null "" -> [] 
  |Null s -> [s]
  |Node(a,t,g,c) -> (collectdata a) @ (collectdata t) @ (collectdata g) @ (collectdata c);; 

let rec sameprefix d seq = 
  match (seq,d) with
  |([],d) -> collectdata d
  |(_::_,Null _) -> failwith "no sequences have this prefixe."
  |(A::r,Node(a,t,g,c)) -> sameprefix a r
  |(T::r,Node(a,t,g,c))-> sameprefix t r 
  |(G::r,Node(a,t,g,c) ) ->sameprefix g r
  |(C::r,Node(a,t,g,c)) -> sameprefix c r;;

(*11*)

let rec headadd base seqs = 
  match seqs with 
  |[] -> []
  |seq::r -> (base::seq)::headadd base r ;;

let rec dico2seqs d = 
  match d with 
  |Null "" -> []
  |Null _ -> [[]]
  |Node (a,t,c,g) -> (headadd A (dico2seqs a)) @ 
                     (headadd T (dico2seqs t)) @
                     (headadd G (dico2seqs g)) @
                     (headadd C (dico2seqs c));;

(*12*)

let rec delete d seq = 
  let clean d = 
    match d with 
    |Node (Null"",Null"",Null"", Null"") -> Null ""
    |d -> d 
  in
  match (seq,d) with 
  | ([], Null s) -> Null"" (*la séquence se termine sur une information à retirer*)
  | ([], d) -> d (*la sequence se termine mais pas sur une information*)
  |(_, Null s) -> Null s (*séquence qui n'est pas sur le dictionnaire*)
  |(A::r , Node(a,t,c,g)) -> clean (Node(delete a r, t,g,c)) 
  |(T::r , Node(a,t,c,g)) -> clean (Node(a,delete t r,g,c)) 
  |(G::r , Node(a,t,c,g)) -> clean (Node(a,t, delete g r, c) )
  |(C::r , Node(a,t,c,g)) -> clean (Node(a,t,g,delete c r)) 

                 
                









