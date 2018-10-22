open Hw1;;

module StringSet = Set.Make (String)

let rec free_vars_t x = match x with
  Var s -> StringSet.singleton s
  | App (n, m) -> StringSet.union (free_vars_t n) (free_vars_t m)
  | Abs (s, x) -> StringSet.remove s (free_vars_t x);;

let rec free_vars x = StringSet.elements (free_vars_t x);;

let rec free_to_subst x y var = match y with 
	Var s -> true
	| App (m, n) -> (free_to_subst x m var) && (free_to_subst x n var)
	| Abs (s, n) -> if s = var then true
		else not (StringSet.mem var (free_vars_t n)) || (not (StringSet.mem s (free_vars_t x)) && free_to_subst x n var);;

let rec is_normal_form x = match x with
	Var x -> true
	| App(Abs(x, r1), r) -> false 
	| App(l, r) -> (is_normal_form l) && (is_normal_form r) 		
	| Abs(x, r) -> is_normal_form r 

let rec substitute x y var = match y with
    Var s -> if s = var then x else y
  	| App (f, a) -> App ((substitute x f var), (substitute x a var))
  	| Abs (s, a) -> if s == var then y else
      	let new_name, new_a =
        	if StringSet.mem s (free_vars_t x) then
          		let new_name = "t" ^ string_of_int (Hashtbl.hash (App (a, x))) in new_name, substitute (Var new_name) a s
        	else s, a
      		in Abs(new_name, substitute x new_a var);;

let rec is_alpha_equivalent x y = match (x, y) with
    (Var a, Var b) -> a = b
  	| (App (n, a), App (m, b)) -> is_alpha_equivalent n m && is_alpha_equivalent a b
  	| (Abs (a, n), Abs (b, m)) -> is_alpha_equivalent n (substitute (Var a) m b)
  	| _ -> false;;


module StringMap = Map.Make(String);;
	
let rec normal_beta_reduction x = match x with
	Var x -> Var x
	| App(Abs(x, n), m) -> substitute n m x
	| App(l, r) -> 
		let l_new = normal_beta_reduction l in
		if l_new != l then 
			App(l_new, r) else
			App(l_new, normal_beta_reduction r)	 
	| Abs(x, r) -> Abs(x, normal_beta_reduction r)

let rec reduce_to_normal_form x = 
	if is_normal_form x then 
        x else 
        reduce_to_normal_form (normal_beta_reduction x);;



