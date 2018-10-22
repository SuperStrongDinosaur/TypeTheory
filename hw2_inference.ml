open Hw1
open Hw2_unify

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type
type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let cnt = ref 0;;

let gen_fresh_name () = 
	cnt := !cnt + 1;
	"t" ^ (string_of_int !cnt );;

let gen_fresh_type () = 
	Var (gen_fresh_name ());;

let infer_simp_type a = 
	cnt := 0;
	let rec build_sys a = 
		let rec convert_sys a eq context goal = match a with
			Hw1.Var x -> (context, (let (_, e) = List.find (fun (t, e) -> t = x) context in e, goal) :: eq)
			| Hw1.Abs(x, lmb) -> let x_type = gen_fresh_type () and lmb_type = gen_fresh_type () in
				convert_sys lmb ((goal, Fun ("f", [x_type; lmb_type])) :: eq) ((x, x_type) :: context) lmb_type
			| Hw1.App(lmb1, lmb2) -> 
				let lmb2_type = gen_fresh_type() in let lmb1_type = Fun ("f", [lmb2_type; goal]) in
				let (x, a) = (convert_sys lmb1 eq context lmb1_type) and 
					(y, b) = (convert_sys lmb2 eq context lmb2_type) in (x @ y, a @ b) in

		let rec delete_dub lst res = match lst with
			[] -> res
			| head :: tail -> if List.mem head res then delete_dub tail res else delete_dub tail (head :: res) in

		let (sys, expr_type) = convert_sys a [] [] (gen_fresh_type ()) in
		(delete_dub sys [], delete_dub expr_type []) in
	let (expr_type, sys) = build_sys a in
	match solve_system sys with
	None -> None
	| Some expr -> 
		let rec type_of_term a = match a with
			Var x -> S_Elem x
			| Fun(f, l :: r :: []) -> S_Arrow(type_of_term l, type_of_term r)
			| _ -> failwith "unexpected algebraic term" in
		Some (List.map (fun (str, term) -> (str, type_of_term term)) (List.map (fun (str, term) -> (str, apply_substitution expr term)) expr_type), 
		let (_, tmp) = List.find (fun (str, term) -> str = "t" ^ "0") expr in type_of_term tmp);;

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

exception SystemException of string;; 

let algorithm_w hm_lam = 	
	cnt := 0;

	let rec algorithm_w_t hmlmb types = 
		let rec try_to_subst s hmt set_of_vars = 
			match hmt with
		  	| HM_ForAll (x, y) -> HM_ForAll(x, try_to_subst s y (StringSet.add x set_of_vars))
		  	| HM_Arrow (x, y) -> HM_Arrow(try_to_subst s x set_of_vars, try_to_subst s y set_of_vars)
			| HM_Elem x -> 
				if StringSet.mem x set_of_vars then hmt
				else if StringMap.mem x s then StringMap.find x s
				else hmt in

		let combine_substs f1 f2 = 
			StringMap.fold 
				(fun x y m -> 
				if StringMap.mem x m then m 
				else StringMap.add x y m) f1 (StringMap.fold (fun x y m -> StringMap.add x (try_to_subst f1 y StringSet.empty) m) f2 StringMap.empty) in

		let merge_subst s tps = StringMap.fold (fun x y m -> StringMap.add x (try_to_subst s y StringSet.empty) m) tps StringMap.empty in

		let var_case v =
			let rec del hmt = 
				match hmt with
				| HM_ForAll(x, y) -> try_to_subst (StringMap.add x (HM_Elem(gen_fresh_name ())) StringMap.empty) (del y) StringSet.empty
			  	| _ -> hmt in
			if StringMap.mem v types
			then (del (StringMap.find v types), StringMap.empty)
			else raise (SystemException "Any type") in

		let abs_case x y =
	  		let fresh_type = HM_Elem (gen_fresh_name ()) in
			let (hmt, types) = algorithm_w_t y (StringMap.add x fresh_type (StringMap.remove x types)) in
			(HM_Arrow(try_to_subst types fresh_type StringSet.empty, hmt), types) in

		let app_case lhs rhs =
		 	let (hmt_left, types_left) = algorithm_w_t lhs types in
			let (hmt_right, types_right) = algorithm_w_t rhs (merge_subst types_left types) in
			let fresh_type = HM_Elem (gen_fresh_name ()) in
			let rec to_aterm hmt = 
				match hmt with
			  	| HM_Arrow(x, y) -> Hw2_unify.Fun ("f",  [(to_aterm x); (to_aterm y)])
				| HM_Elem v -> Hw2_unify.Var v
			  	| _ -> failwith "Wut"  in
			match solve_system ([((to_aterm (try_to_subst types_right hmt_left StringSet.empty)), (to_aterm (HM_Arrow(hmt_right, fresh_type))))]) with
		  	| Some x -> 
		  		let rec from_aterm term = 
					match term with
				  	| Hw2_unify.Fun(name, [x; y]) -> HM_Arrow (from_aterm x, from_aterm y)
					| Hw2_unify.Var v -> HM_Elem v
				  	| _ -> failwith "Wut" in	
		  		let finale_types = combine_substs 
		  		(List.fold_left (fun mp (s, term) -> StringMap.add s (from_aterm term) mp) StringMap.empty x) (combine_substs types_right types_left) in
				(try_to_subst finale_types fresh_type StringSet.empty, finale_types)
			| None -> raise (SystemException "Incompatible system") in

		let let_case x y z =
			let _add hmt types = 
				let rec unblock hmt blocked = 
					match hmt with
					| HM_Elem v -> if StringSet.mem v blocked then StringSet.empty else StringSet.singleton v
				  	| HM_Arrow (x, y) -> StringSet.union (unblock x blocked) (unblock y blocked)
				  	| HM_ForAll (x, y) -> unblock y (StringSet.add x blocked) in
				let get_known_types tps =  StringMap.fold (fun extra x s -> StringSet.union (unblock x StringSet.empty) s) tps StringSet.empty in
				let known_types = get_known_types types in
				StringSet.fold (fun x y -> HM_ForAll(x, y)) 
				(StringSet.fold (fun x y -> match StringSet.mem x known_types with
					| false -> StringSet.add x y
					| true -> y)
				(unblock hmt StringSet.empty) StringSet.empty) hmt in	
	  		let (hmt1, types1) = algorithm_w_t y types in 
			let fresh_types = merge_subst types1 types in 
			let (hmt2, types2) = algorithm_w_t z (StringMap.add x (_add hmt1 fresh_types) (StringMap.remove x fresh_types)) in 
			(hmt2, combine_substs types2 types1) in

		match hmlmb with
		| HM_Var v -> var_case v
	  	| HM_Abs (x, y) -> abs_case x y
	  	| HM_App (lhs, rhs) -> app_case lhs rhs
	  	| HM_Let (x, y, z) -> let_case x y z in

	let types = StringSet.fold (fun x map -> StringMap.add x (HM_Elem (gen_fresh_name ())) map)
	((fun x -> 
		let rec f hmlmb set = 
			match hmlmb with
			| HM_Var v -> if StringSet.mem v set then StringSet.empty else StringSet.singleton v
		  	| HM_Abs (x, y) -> f y (StringSet.add x set)
		  	| HM_App (lhs, rhs) -> StringSet.union (f lhs set) (f rhs set)
		  	| HM_Let (x, y, z) -> StringSet.union (f y set) (f z (StringSet.add x set)) in 
		f x StringSet.empty) hm_lam) StringMap.empty in
	try let (hmt, map) = algorithm_w_t hm_lam types in 
	Some (StringMap.bindings map, hmt) with
	(SystemException sww) -> None;;