(* KONSTRUKTORY *)

type wartosc = float * float;;

let wartosc_dokladnosc x p = (( x -. p *. (abs_float x) /. 100., x +. p *. (abs_float x) /. 100.):wartosc);;
let wartosc_od_do x y = ((x, y):wartosc);;
let wartosc_dokladna x = ((x, x):wartosc);;

(* Funkcje pomocnicze zwracajaca minimum lub maksimum z 4 wartosci *)
let is_nan x = compare x nan = 0;;
let minn a b =
    if is_nan a then b
    else if is_nan b then a
    else min a b
;;
let maxx a b =
    if is_nan a then b
    else if is_nan b then a
    else max a b
;;
let minimum a b c d = minn a (minn b (minn c d));;
let maksimum a b c d = maxx a (maxx b (maxx c d));;
(* Funkcja pomocnicza wyznaczajaca dla zbiorow A B zbior A U B, A i B sa od neg_infinity albo do infinity *)

let rec scal (a:wartosc) (b:wartosc) =
        if (fst a = neg_infinity && snd b = infinity)
            then if snd a >= fst b
                then (neg_infinity, infinity)
                else (fst b, snd a)
            else if (fst b = neg_infinity && snd a = infinity)
                then if snd b >= fst a
                    then (infinity, neg_infinity)
                    else (fst a, snd b)
                else if fst a > snd a (* przypadek zbioru z luka *)
                    then if fst b > snd b (*dwa zbiory z luka*)
                        then if (snd b >= fst a || snd a >= fst b) (*czy zbiory maja rozlaczne luki *)
                            then ((neg_infinity, infinity):wartosc)
                            else (min (fst a) (fst b), (max (snd a) (snd b)):wartosc)
                        else if snd b = infinity (* jeden zbior z luka i jeden od pewnego punktu do neg_infinity albo infinity *)
                            then if fst b <= snd a
                                then ((neg_infinity, infinity):wartosc)
                                else (snd a, min (fst a) (fst b))
                            else if snd b >= fst a (* analog do 3 linijki w gore *)
                                then ((neg_infinity, infinity):wartosc)
                                else (fst a, max (snd a) (snd b))
                    else if fst b > snd b
                        then scal b a (* jezeli b jest z luka, to ponowne wywolanie funkcji "odpali" pierwszego if'a*)
                        else if (fst a = neg_infinity && snd a = infinity) || (fst b = neg_infinity && snd b = infinity) (* żaden ze zbiorow nie ma luki*)
                            then ((neg_infinity, infinity):wartosc)
                            else if snd a = infinity
                                then if snd b = infinity
                                    then ((min (fst a) (fst b), infinity):wartosc)
                                    else if snd b >= fst a
                                        then ((neg_infinity, infinity):wartosc)
                                        else ((fst a, snd b):wartosc)
                                else if fst b = neg_infinity
                                    then ((neg_infinity, max (snd a) (snd b)):wartosc)
                                    else scal b a
;;

(* SELEKTORY *)

let in_wartosc (x:wartosc) (y:float) = 
        if (is_nan (fst x) || is_nan (snd x))
            then false
            else if fst x > snd x
                then (y >= (fst x) || y <= (snd x))
                else (y >= (fst x) && y <= (snd x))
;;
let min_wartosc (x:wartosc) =
        if is_nan (fst x) || is_nan (snd x)
            then nan
            else if fst x > snd x
                then neg_infinity
                else fst x
;;
let max_wartosc (x:wartosc) =
        if is_nan (fst x) || is_nan (snd x)
            then nan
            else if fst x > snd x
                then infinity
                else snd x
;;
let sr_wartosc (x:wartosc) =
        if is_nan (fst x) || is_nan (snd x)
            then nan
            else if fst x > snd x
                then nan
                else (min_wartosc x +. max_wartosc x) /. 2.
;;

(* MODYFIKATORY *)

let plus (a:wartosc) (b:wartosc) =
        if fst a > snd a 
            then if fst b > snd b 
                then ((neg_infinity, infinity):wartosc) (* suma dwoch zbiorow z "luka" sumuja sie do zbioru od infinity do neg_infinity *)
                else if (fst a -. snd a <= snd b -. fst b) 
                    then ((neg_infinity, infinity):wartosc) (* jak luka w a jest co najwyjzej wielkosci prz b to uda nam sie ja "zalatac" *)
                    else ((fst a +. fst b, snd a +. snd b):wartosc) (* latamy ile sie da *)
            else if fst b > snd b 
                then if (fst b -. snd b <= snd a -. fst a) (* analog do 5 linii w gore *)
                    then ((neg_infinity, infinity):wartosc)
                    else  ((fst a +. fst b, snd a +. snd b):wartosc)
                else  ((fst a +. fst b, snd a +. snd b):wartosc) (* dwa skonczone przedzialy *)
;;

let minus (a:wartosc) (b:wartosc) = plus a (-. snd b, -. fst b);;

let rec razy (a:wartosc) (b:wartosc) =
        if (is_nan (fst a) || is_nan (fst b)) then ((nan, nan):wartosc) 
        else if (a = wartosc_dokladna 0. || b = wartosc_dokladna 0.) 
            then wartosc_dokladna 0.
            else if fst a <= snd a 
                then if fst b <= snd b 
                    then ((minimum (fst a *. fst b) (fst a *. snd b) (snd a *. fst b) (snd a *. snd b), maksimum (fst a *. fst b) (fst a *. snd b) (snd a *. fst b) (snd a *. snd b)):wartosc)
                    else if in_wartosc a 0.
                        then ((neg_infinity, infinity):wartosc)
                else scal (razy ((neg_infinity, snd b):wartosc) a) (razy ((fst b, infinity):wartosc) a)
            else if fst b <= snd b
            then razy b a
            else scal (scal (razy ((neg_infinity, snd a):wartosc) ((neg_infinity, snd b):wartosc)) (razy ((neg_infinity, snd a):wartosc) (fst b, infinity):wartosc)) (scal (razy ((fst a, infinity):wartosc) ((neg_infinity, snd b):wartosc)) (razy ((fst a, infinity):wartosc) (fst b, infinity):wartosc))
;;

let rec podzielic (b:wartosc) (a:wartosc) = (* dziele zbior b na a*)
    if is_nan (fst a) || is_nan (fst b) 
        then ((nan, nan):wartosc)
        else if a = wartosc_dokladna 0. 
            then ((nan, nan):wartosc)
            else if fst a <= snd a
                then if fst a = 0.
                    then razy (((1. /. (snd a)),  infinity):wartosc) b
                    else if snd a = 0.
                        then razy (neg_infinity, (1. /. (fst a))) b
                        else if (in_wartosc a 0.) 
                            then scal ((podzielic b ((fst a, 0.):wartosc))) (podzielic b ((0. , snd a):wartosc))
                            else razy b ((min (1. /. (fst a)) (1. /. (snd a))), (max (1. /. (fst a)) (1. /. (snd a))):wartosc)
                else if snd a = 0. 
                    then razy b ((neg_infinity, 1. /. (fst a)):wartosc)
                    else if snd a > 0.
                        then razy b (((1. /. snd a), (1. /. fst a)):wartosc)
                        else if fst a >= 0.
                            then razy b (((1. /. snd a), (1. /. fst a)):wartosc)
                            else razy b (((1. /. snd a), (1. /. fst a)):wartosc)
;;
