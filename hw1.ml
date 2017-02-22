type peano = Z | S of peano;; (* òèïû íåîáõîäèìî êîïèðîâàòü â ðåàëèçàöèþ *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with 
	0 -> Z
	| _ -> S(peano_of_int (x - 1));;

let rec int_of_peano p = match p with
    Z -> 0
	| S x -> 1 + int_of_peano x;;

let rec inc x = S x;;

let rec add x y = match y with
	Z -> x
	| S c -> S(add x c);;

let dec x = match x with
    Z -> Z
    | S b -> b;; 

let rec sub x y = match y with
	Z -> x
	| S c -> sub (dec x) c

let rec mul x y =  match y with
	Z -> Z
	| S c -> add x (mul x c);;

let rec div x = failwith "Not implemented";;

let rec power x y = match y with
	Z -> S Z
	| S c -> mul x (power x c);;
                     
let rec rev x = failwith "Not implemented";;

let rec merge_sort x = failwith "Not implemented";;
                     
let rec string_of_lambda x = failwith "Not implemented";;
let rec lambda_of_string x = failwith "Not implemented";;