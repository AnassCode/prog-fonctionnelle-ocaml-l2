type mois_t = Jan|Feb|Mar|Apr|May|Jun|Jul|Aout|Sep|Oct|Nov|Dec ;; 

type date_t = {jour:int ; mois:mois_t ; an:int};;

let correct_month day month =
  match month with
  |Jan | Mar | May | Jul | Aout | Oct | Dec when (day >= 1 && day <= 31) -> true
  |Apr | Jun | Sep | Nov when (day >= 1 && day <= 30) -> true
  |Feb when (day >= 1 && day <= 29) -> true 
  |_ -> false;; 

let correct_date d = if (d.an>1900 && d.an<= 2025) && (correct_month d.jour d.mois) then true else false;;

type personne = {nom : string ; prenom : string ; date_naissance : date_t ; num : string };;

let rec add personne bd = 
  match bd with 
  |[] -> [personne]
  |x::r when x.nom > personne.nom -> personne::bd
  |x::r when x.nom = personne.nom -> personne::bd
  |x::r -> x::add personne r;;
  
let rec delete personne bd =
  match bd with 
  |[] ->  []
  |x::r when x = personne -> r
  |x::r -> x::delete personne r;;

let rec find personne bd = 
  match bd with 
  |[] -> false 
  |x::r when x=personne -> true 
  |x::r -> find personne r;;
  
let rec douze_mars bd = 
  match bd with 
  |[] -> []
  |x::r when (x.date_naissance.jour == 12) && (x.date_naissance.mois == Mar) -> x::douze_mars r
  |x::r -> douze_mars r;;

let rec debut_D bd = 
  match bd with 
  |[] -> [] 
  |x::r when x.nom.[0] == 'D' -> x::debut_D r
  |x::r -> debut_D r;;

let rec fusion bd1 bd2 = 
  match bd1,bd2 with 
  |([],bd2) -> bd2
  |(bd1,[]) -> bd1
  |(a1::r1,a2::r2) when a1<a2 -> a1::(fusion r1(a2::r2))
  |(a1::r1,a2::r2) when a1>a2 -> a2::(fusion(a1::r1)r2)
  |(a1::r1,_::r2) -> a1::(fusion r1 r2);; 

let rec select bd binop = 
  match bd with 
  |[] -> []
  |x::r when binop x -> x::select r binop
  |x::r -> select r binop;;

let select1 x = if (x.date_naissance.jour == 12) && (x.date_naissance.mois == Mar) then true else false;;
let select2 x = if (x.nom.[0] == 'D') then true else false;;

let rec select question format l = 
  match l with 
  |[] -> [] 
  |x::r when question x -> format x :: select question format r
  |x::r -> select question format r 

let format x = (x.nom,x.prenom) ;;


    

    

    
    
    
    
    
    
  