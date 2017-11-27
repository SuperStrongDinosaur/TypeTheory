open Hw1;;

module StringSet = Set.Make (String);;

let free_vars l = 
	let rec set_of_free_vars l = 
		match l with
			| Var x -> StringSet.add x StringSet.empty
			| Abs (x, n) -> StringSet.remove x (set_of_free_vars n)
			| App (m, n) -> StringSet.union (set_of_free_vars m) (set_of_free_vars n) 
		in
	let s = set_of_free_vars l in
	StringSet.elements s;;


let free_to_subst x = failwith "Not implemented";;
let is_normal_form x = failwith "Not implemented";;
let is_alpha_equivalent x = failwith "Not implemented";;
let normal_beta_reduction x = failwith "Not implemented";;
let reduce_to_normal_form x = failwith "Not implemented";;


let t5 = "(\\f.\\x.f (f (f (f x)))) (\\f.\\x.f (f (f (f x))))";;
let printlist l = List.iter (fun x -> print_string x) (free_vars (Hw1.lambda_of_string t5));;
	

