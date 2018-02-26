type algebraic_term = Var of string | Fun of string * (algebraic_term list);;

let system_to_equation sys =
  let name = "F_" ^ string_of_int (Hashtbl.hash sys) in
  let l = List.map (fun (l, r) -> l) sys in
  let r = List.map (fun (l, r) -> r) sys in
  Fun (name, l), Fun (name, r);;

let rec apply_substitution sys var = match var with
	Var x -> (try let (f, y) = List.find (fun (t, eq) -> t = x) sys in y with Not_found -> var)
	| Fun (f, lst) -> Fun (f, List.map (fun arg -> apply_substitution sys arg)  lst);; 

let rec check_equals n m = match (n, m) with
	(Var x, Var y) -> x = y
	| (Fun(f, x), Fun(g, y)) -> f = g && List.for_all2 check_equals x y
	| _ -> false;;

let check_solution n m = List.for_all (fun (t1, t2) -> check_equals (apply_substitution n t1) (apply_substitution n t2)) m;; 

exception Not_solved;;

let rec check_used var eq = match eq with
	Var x -> x = var
	| Fun (f, x) -> List.exists (check_used var) x;;

let map_terms var t  = fun (x, y) -> (apply_substitution [(var, t)] x, apply_substitution [(var, t)] y);;

let rec unify lst1 lst2 = match lst1 with
	[] -> List.map (fun (t1, t2) -> match (t1, t2) with 
			(Var x, _) -> (x, t1) 
			| _ -> failwith "Error in unify") lst2
	| (t1, t2) :: t -> if check_equals t1 t2 then 
			unify t lst2
		else
			match (t1, t2) with
				(Var x, _) ->  if check_used x t2 then 
						raise Not_solved
					else
						unify (List.map (map_terms x t2) lst1) ((t1, t2) :: (List.map (map_terms x t2) lst2))
				| (Fun(f, x), Var y) -> unify ((t2, t1) :: t) lst2
				| (Fun(f, x), Fun(g, y)) -> if f = g then 
						(try let decomposed = List.combine x y in unify (decomposed @ t) lst2
				with Invalid_argument msg -> raise Not_solved)
			else raise Not_solved;;

let solve_system sys = (try Some (unify sys []) with Not_solved -> None);;