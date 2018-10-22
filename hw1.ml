type peano = Z | S of peano;; (* òèïû íåîáõîäèìî êîïèðîâàòü â ðåàëèçàöèþ *)

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

let rec power x y = match y with
	  Z -> S Z
	| S c -> mul x (power x c);;

let rec reverse x y = match y with
	  [] -> x
	| head::tail -> reverse(head::x) tail;;
                     
let rec rev x = reverse [] x;;

let rec merge_sort x = 
	let rec merge_sort_impl x l r = 
		if((l + 1) >= r) then (List.nth x l) :: []
		else 
			let rec merge (x, y) = match (x, y) with
				| (a, []) -> a
				| ([], a) -> a
				| (a_head::a, b_head::b) -> 
				if (a_head > b_head) then
					b_head::merge (x, b)
				else 
					a_head::merge (a, y) in 
      		merge ((merge_sort_impl x l ((l + r) / 2)), (merge_sort_impl x ((l + r) / 2) r)) in      
	merge_sort_impl x 0 (List.length x);;


type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;
                     
let rec string_of_lambda x = match x with
	  Var x -> x
	| Abs (x, y) -> "(\\" ^ x ^ "." ^ string_of_lambda y ^ ")"
	| App (x, y) -> "(" ^ string_of_lambda x ^ " " ^ string_of_lambda y ^ ")";;

let lambda_of_string str = 
	let str = str ^ ";" in 
	let pos = ref 0 in
	let next () = if !pos < String.length str - 1 then pos := !pos + 1 in
	let rec ws () = if ((str.[!pos] = ' ') && (!pos < String.length str - 1)) then (next (); ws()) in
	let get () = ws(); str.[!pos] in
	let get_with_WP () = str.[!pos] in
	let eat x = if get_with_WP () = x then next () else failwith ("Unexpected symbols" ^ (String.make 1 (get_with_WP())) ^ string_of_int(!pos)) in
	let rec string_eater s = 
		if (get_with_WP ()) <> ';' && (get_with_WP ()) <> ')' && (get_with_WP ()) <> ' ' && (get_with_WP ()) <> '\\' && (get_with_WP ()) <> '(' && (get_with_WP ()) <> '.' then 
			(let current = s ^ (String.make 1 (get_with_WP())) in next(); string_eater current)
		else s in
		let rec parse () = 
			let rec parse_conditional () = match (get()) with
				'(' -> bracket_parse ()
				| '\\' -> parse_abs ()
				|_ -> var_parse ()
		
		and bracket_parse () = eat '('; let tmp = parse() in eat ')'; tmp
		and parse_abs () = eat '\\'; let nameStr = string_eater "" in eat '.'; Abs(nameStr, parse())		
		and var_parse () = Var(string_eater "") 		
		and parse_app lam  = App(lam, parse_conditional())	in
		let coll = ref (parse_conditional()) in
		while (!pos < String.length str - 1 && (get() <> ')')) do
			coll := parse_app(!coll);
		done;
		!coll
	in parse();;