open Hw1;;

module StringSet = Set.Make (String)

let rec free_vars_t x = match x with
    Var s -> StringSet.singleton s
  | App (n, m) -> StringSet.union (free_vars_t n) (free_vars_t m)
  | Abs (s, x) -> StringSet.diff (free_vars_t x) (StringSet.singleton s);;

let rec free_vars x = StringSet.elements (free_vars_t x);;

let rec free_to_subst x y var = match y with 
	Var s -> true
	| App (m, n) -> (free_to_subst x m var) && (free_to_subst x n var)
	| Abs (s, n) -> if s = var then true
		else not (StringSet.mem var (free_vars_t n)) || (not (StringSet.mem s (free_vars_t x)) && free_to_subst x n var);;

let is_normal_form x =
	let rec no_beta_redex x =
		match x with
			Var x -> true
			| App(Abs(x, rinner), r) -> false 
			| App(l, r) -> (no_beta_redex l) && (no_beta_redex r) 		
			| Abs(x, r) -> no_beta_redex r in
	no_beta_redex x;;

let rec substitute x y var = match y with
    Var s -> if s = var then x else y
  	| App (f, a) -> App ((substitute x f var), (substitute x a var))
  	| Abs (s, a) -> if s == var then y else
      	let new_name, new_a =
        	if StringSet.mem s (free_vars_t x) then
          		let new_name = "t_" ^ string_of_int (Hashtbl.hash (App (a, x))) in new_name, substitute (Var new_name) a s
        	else s, a
      		in
     		Abs(new_name, substitute x new_a var);;

let rec is_alpha_equivalent x y = match (x, y) with
    (Var a, Var b) -> a = b
  	| (App (n, a), App (m, b)) -> is_alpha_equivalent n m && is_alpha_equivalent a b
  	| (Abs (a, n), Abs (b, m)) -> is_alpha_equivalent n (substitute (Var a) m b)
  	| _ -> false;;


module StringMap = Map.Make(String);;

let rename_arguments x =
	let counter = ref 0 in
	let get_fresh_name () =
		counter := !counter + 1;
		"t" ^ (string_of_int !counter) in 
	let rec hepler x map =
		match x with
			Var v -> if (StringMap.mem v map) then (Var (StringMap.find v map)) else x
			| Abs(v, l) ->  
				(let nn = get_fresh_name () in
				(Abs(nn, hepler l (StringMap.add v nn map)))) 
			| App(lr, ll) -> App(hepler lr map, hepler ll map) in
	hepler x StringMap.empty;;
	
let normal_beta_reduction_t x =
	let rec impl x =
		match x with
			Var x -> (false, Var x)
			| App(Abs(x, n), m) -> (true, substitute n m x)
			| App(l, r) -> 
				let flag, l_new = impl l in
				if flag then 
					(true, App(l_new, r)) else
					let flag, r_new = impl r in
					(flag, App(l_new, r_new)) 
			| Abs(x, r) -> 
				let flag, l_new = impl r in
				(flag, Abs(x, l_new)) in
	let has_happened, new_lm = impl x in new_lm;;

let normal_beta_reduction x = normal_beta_reduction_t (rename_arguments x);;

let reduce_to_normal_form x = 
	let rec impl x =
		if is_normal_form x
			then x
			else impl (normal_beta_reduction x) in impl (rename_arguments x);;



