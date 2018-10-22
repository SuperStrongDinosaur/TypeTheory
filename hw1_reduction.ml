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


let cnt = ref 0;;
let map_ref = StringMap.empty;;

type lambda_ref = VarRef of string | AbsRef of string * lambda_ref ref | AppRef of lambda_ref ref * lambda_ref ref;; 

let rec ref_from_lambda a = match a with
    Var x -> ref (VarRef x)
    | App (x, y) -> ref (AppRef (ref_from_lambda x, ref_from_lambda y))
    | Abs (x, y) -> ref (AbsRef (x, ref_from_lambda y));;

let rec lambda_from_ref a_ref = match !a_ref with
    VarRef x -> Var x
    | AppRef (x, y) -> App (lambda_from_ref x, lambda_from_ref y)
    | AbsRef (x, y) -> Abs (x, lambda_from_ref y);;

let gen_fresh_name () = 
    cnt := !cnt + 1;
    "t" ^ (string_of_int !cnt );;

let rename a = 
    let rec rename_t a map = match a with
        Var x -> if StringMap.mem x map then Var (StringMap.find x map) else Var x
        | App (x, y) -> App (rename_t x map, rename_t y map)
        | Abs (x, y) ->  let new_var = gen_fresh_name () in Abs (new_var, rename_t y (StringMap.add x new_var map)) in
    rename_t a map_ref;;

let is_term_beta_redex_ref a_ref = match !a_ref with
    AbsRef (_, _) -> false
    | VarRef x -> false
    | AppRef (x, y) -> match !x with
        AbsRef (_, _) -> true
        | _ -> false;;

let rec is_normal_form_ref a_ref = 
    if is_term_beta_redex_ref a_ref then false else match !a_ref with
    | VarRef x -> true
    | AppRef (x, y) -> (is_normal_form_ref x) && (is_normal_form_ref y)
    | AbsRef (x, y) -> is_normal_form_ref y;;

let rec subst_ref a b var = match !b with
    VarRef x -> if x = var then b := (!a)
    | AbsRef (x, y) -> if x != var then subst_ref a y var
    | AppRef (x, y) -> (subst_ref a x var); (subst_ref a y var);;

let rec normal_beta_reduction_ref a_ref = match !a_ref with
    VarRef x -> ()
    | AbsRef (x, y) -> normal_beta_reduction_ref y
    | AppRef (x, y) -> match !x with
        AbsRef (o, z) -> a_ref := (!(ref_from_lambda (rename (lambda_from_ref z)))); subst_ref y a_ref o
        | _ -> if is_normal_form_ref x then (normal_beta_reduction_ref y) else (normal_beta_reduction_ref x);;

let reduce_to_normal_form a = 
    let rec reduce_to_normal_form_ref a_ref = 
    if is_normal_form_ref a_ref then 
        a_ref 
    else   
        reduce_to_normal_form_ref (normal_beta_reduction_ref a_ref; a_ref) in
    lambda_from_ref (reduce_to_normal_form_ref (ref_from_lambda (rename a)));;



