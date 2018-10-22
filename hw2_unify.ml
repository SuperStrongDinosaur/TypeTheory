type algebraic_term = Var of string | Fun of string * (algebraic_term list);;

let system_to_equation sys =
  let name = "F" ^ string_of_int (Hashtbl.hash sys) in
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

module StringMap = Map.Make(String)

let solve_system sys =
  let rec solve_t sys sol = match sys with
    [] -> Some (StringMap.bindings sol)
    | head :: tail ->
      match head with l, r ->
        if l = r then solve_t tail sol
        else match head with
          | Var x, any ->
          	let rec member var term =
			  match term with
			    Var name -> name = var
			  | Fun (f, args) -> List.fold_left (fun last arg -> last || member var arg) false args in
            if member x any then None
            else
              let new_sol = StringMap.add x any (StringMap.map (fun term -> apply_substitution [(x, any)] term) sol) in
              let do_sub = apply_substitution (StringMap.bindings new_sol) in
              solve_t (List.map (fun (l, r) -> do_sub l, do_sub r) tail) new_sol
          | any, Var x -> solve_t ((Var x, any) :: tail) sol
          | Fun(f, f_args), Fun(g, g_args) ->
            if f <> g then None
            else solve_t (List.fold_left2 (fun tail fa ga -> (fa, ga) :: tail) tail f_args g_args) sol
  in solve_t sys StringMap.empty;;