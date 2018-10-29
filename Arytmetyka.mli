(* KONSTRUKTORY *)

type wartosc = float * float;;
(*W zadaniu jedynymi mozliwymi do otrzymania zbiorami jest zbior spojny lub zbior od neg_infinity do infinity z "luka", dlatego spojny przedzial (a, b) oznaczam przez uparzadkawana pare a<=b, natomiast zbior z luka oznaczam przez (b, a), gdzie a<b i oznacza to zbior (neg_infinity, b)U(a, infinity) *)
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
(* Funkcja pomocnicza wyznaczajaca dla zbiorow A B zbior A U B, A i B sa albo zbiorami z luka albo ktorys z ich koncow jest w neg_infinity albo w infinity *)

let rec scal (a:wartosc) (b:wartosc) =
        if (fst a = neg_infinity && snd b = infinity) (* a = (neg_infinity, x) b = (y, infinity), najpierw rozpatruje przypadki gdy a nie ma luki *)
            then if snd a >= fst b (* przedzialy albo sie pokrywaja albo zostawiaja luke *)
                then (neg_infinity, infinity)
                else (fst b, snd a)
            else if (fst b = neg_infinity && snd a = infinity) (*if analog do 1. ifa tylko w odwrotnej kolejnosci, wystarczy odpalic scal b a *)
                then scal b a
                else if snd a = infinity && snd b = infinity (*oba przedzialy do infinity *)
                    then (min (fst a) (fst b), infinity)
                    else if fst a = neg_infinity && fst b = neg_infinity (*oba przedzialy od neg_infinity *)
                        then (neg_infinity, max (snd a) (snd b))
                        else if fst b = neg_infinity  (* a ma luke, rozpatruje jakie moze byc b *)
                            then scal (scal b (neg_infinity, snd a)) (fst a, infinity) (*b od neg_infinity *)
                            else if snd b = infinity (*b jest do infinity*)
                                then scal (neg_infinity, snd a) (scal b (fst a, infinity))
                                else scal (scal (neg_infinity, snd a) (neg_infinity, snd b)) (scal (fst a, infinity) (fst b, infinity)) (*b jest z luka*)
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

let rec plus (a:wartosc) (b:wartosc) =
        if fst a > snd a (* a ma luke *)
            then if fst b > snd b  (*b ma luke *)
                then (neg_infinity, infinity) (* suma dwoch zbiorow z "luka" zawsze sumuja sie do zbioru od infinity do neg_infinity *)
                else if (fst a -. snd a <= snd b -. fst b) (*a ma luke, b nie, patrzymy czy da sie luke "zalatac" -> przedzial b musi byc wielkosci co najmniej luki a *)
                    then (neg_infinity, infinity)
                    else (fst a +. fst b, snd a +. snd b) (* latamy ile sie da *)
            else if fst b > snd b (* b ma luke, a nie *)
                then plus b a (*rozpatrzylismy juz przypadek, gdy pierwszy zbior ma luke*)
                else  (fst a +. fst b, snd a +. snd b) (* dwa zbiory bez luki *)
;;
(* plus dziala dla ktoregos przedzialu pustego (nan, nan), bo zawsze dojdziemy do sumowania krancow przedzialow, a nan+cos=nan *)
let minus a b = plus a (-. snd b, -. fst b);;
(* odejmowanie przedzialow to dodawanie przedzialu przeciwnego *)
let rec razy a b =
        if (is_nan (fst a) || is_nan (fst b)) then (nan, nan) (*sprawdzamy nany*)
        else if (a = wartosc_dokladna 0. || b = wartosc_dokladna 0.) (*ktorys przedzial to (0,0) to wyjdzie zawsze (0,0), pomimo, ze o.*.infinity = nan, to 0 i tak zawsze bedzie zawarte w przedziale wynikowym *)
            then wartosc_dokladna 0.
            else if fst a <= snd a  (*a nie ma luki *)
                then if fst b <= snd b (*b nie ma luki*)
                    then (minimum (fst a *. fst b) (fst a *. snd b) (snd a *. fst b) (snd a *. snd b), maksimum (fst a *. fst b) (fst a *. snd b) (snd a *. fst b) (snd a *. snd b))
                    else if in_wartosc a 0. (*a nie ma luki, b ma, jak a zawiera 0, to moge uzyskac kazda liczbe*)
                        then (neg_infinity, infinity)
                else scal (razy (neg_infinity, snd b) a) (razy (fst b, infinity) a) (*a ma luke*)
            else if fst b <= snd b (*b nie ma luki, dal tego przypadku rozwazylismy przypadek odwrotny, razy b a zwroci poprawny wynik*)
            then razy b a
            else scal (scal (razy (neg_infinity, snd a) (neg_infinity, snd b)) (razy (neg_infinity, snd a) (fst b, infinity))) (scal (razy (fst a, infinity) (neg_infinity, snd b)) (razy (fst a, infinity) (fst b, infinity))) (* rozbijam zbiory z luka na dwa zbiory bez luki, mnoze je ze soba i wyniki scalam *)
;;

let rec podzielic a b = (* dziele zbior a na b*)
    if is_nan (fst b) || is_nan (fst a) (*sprawdzam nany*)
        then (nan, nan)
        else if b = wartosc_dokladna 0. 
            then (nan, nan) (*nie dzielimy przez 0*)
            else if fst b <= snd b (*b jest bez luki *)
                then if fst b = 0. (* b zaczyna sie od zera, nie zawiera wartosci ujemnych*)
                    then razy ((1. /. (snd b)),  infinity) a (*dzielenie to mnozenie przez odwrotnosc*)
                    else if snd b = 0. (*przedzal b konczy sie na 0, nie zawiera liczb dodatnich*)
                        then razy (neg_infinity, (1. /. (fst b))) a (*mnozenie przez odwrotnosc*)
                        else if (in_wartosc b 0.) (* a zawiera liczby ujemne i dodatnie, scalam wyniki dla dzielenia przez przedzialy (fst a, 0) i (0, snd a)*)
                            then scal ((podzielic a (fst b, 0.))) (podzielic a (0. , snd b))
                            else razy a (min (1. /. (fst b)) (1. /. (snd b)), max (1. /. (fst b)) (1. /. (snd b))) (*mnoze przez odwrotnosc*)
                else if snd b = 0. (*b jest z luka, luka zaczyna sie na 0, mnozne przez odwrotnosc, if potrzebny bo przedzial jest do 0. a nie -0., nie otrzymamy neg_infinity*)
                    then razy a (neg_infinity, 1. /. (fst b))
                    else razy a ((1. /. snd b), (1. /. fst b)) (*w pozostalych przypadkach bezpiecznie moge mnozyc przez odwrotnosc*)
;;
