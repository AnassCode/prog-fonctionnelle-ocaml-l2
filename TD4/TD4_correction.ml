(*Base de donnée*)

(*1*) 
type mois_t = Jan|Feb|Mars|Apr|May|June|Jul|Aout|Sep|Oct|Nov|Dec ;; 

type date_t = {jour:int ; mois:mois_t ; an:int};;

(*2*) 
let moisjour m = 
  match m with 
  | Jan -> 31
  | Feb -> 29 (*peut etre amélioré*)
  | Mars -> 31
  | Apr -> 30
  | May -> 31
  | June -> 30
  | Jul -> 31
  | Aout -> 31
  | Sep -> 30
  | Oct -> 31
  | Nov -> 30
  | Dec -> 31;;

let correct_date d = 1900 <= d.an && d.an <= 2024 && 1 <= d.jour && d.jour <= moisjour d.mois;;

(*3*) 
type personne_t = {nom:string ; prenom:string ; tel:int ; date:date_t};;

(*4*) 
let rec add bd enr = 
  match bd with 
  |[] -> [enr]
  |a::r when a.nom >enr.nom -> a::add r enr 
  |a::r when a.nom = enr.nom -> enr::r
  |bd -> enr ::bd ;; 

let rec delete bd name = 
  match bd with
  |[] -> []
  |a::r when a.nom = name -> r 
  |a::r when a.nom < name -> a::delete r name 
  |bd -> bd ;;

let rec find bd name = 
  match bd with 
  |a::_ when a.nom = name -> a
  |a::r when a.nom < name -> find r name 
  |_ -> failwith "Not Found" ;;

(*5*)
let rec mars12 bd p = 
  match bd with 
  | [] -> []
  | a::r when p.date.jour = 12 && p.date.mois = Mars -> a::(mars12 r p)
  |_::r -> mars12 r p;;

let rec startD bd = 
  match bd with 
  | [] -> []
  | a::r when String.length a.nom>0 && a.nom.[0] == 'D' -> a::(startD r)
  |_::r -> startD r;;

(*6*)
let rec fusion bd1 bd2 =
  match bd1, bd2 with
  | [], bd2 -> bd2
  | bd1, [] -> bd1
  | a1::r1, a2::r2 when a1.nom < a2.nom -> a1 :: fusion r1 bd2
  | a1::r1, a2::r2 when a1.nom = a2.nom -> a1 :: fusion r1 r2
  | a1::r1, a2::r2 -> a1 :: fusion bd1 r2;;

(*extension des operations sur les bases de données*)

(*1*)
let rec select question l = match l with 
  |[] -> []
  |a::r when (question a) -> a:: select question r 
  |_::r -> select question r;;

(*2*)
let rec select question format l = 
  match l with 
  |[] -> []
  |a::r when (question a) -> (format a):: select question format r
  |_::r -> select question format r;;





