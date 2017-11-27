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

let rec div x = failwith "Not implemented";;

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
	| Abs (x, y) -> "(%" ^ x ^ "." ^ string_of_lambda y ^ ")"
	| App (x, y) -> "(" ^ string_of_lambda x ^ " " ^ string_of_lambda y ^ ")";;

let lambda_of_string s = 
	let s = s ^ ";" in
	let pos  = ref 0 in
	let get() = s.[!pos] in
	let next() = if !pos < String.length s - 1 then pos := !pos + 1 else failwith "oops" in
	let eat x  = if get() <> x then failwith "stop eating" else next() in

	let parse_ident_str()  = 
		let rec  rec_parse s = 
			if ((get() >='0') && (get() <= '9')) then
				let cur_dig = get() in
			 	next();
				rec_parse  (s^ (String.make 1 (cur_dig))) 
				else s in
		let cur =  String.make 1 (get ()) in
		next();
		rec_parse cur in 
	
	let parse_ident() = 
	        Var(parse_ident_str()) in

	let rec parse_abs() = 
		eat '\\';
		let v = parse_ident_str () in
		eat '.';
		let l = parse_lambda() in
		Abs(v, l) 

	and  parse res  =  if ((!pos =  String.length s - 1) || ')' = get())
				then res else  App(res, parse_lambda()) 
	and parse_lambda() = 
		match (get()) with 
		'\\' -> (let res = parse_abs() in
				parse res)
		|'(' -> (eat '('; 
			let res = parse_lambda() in
			eat ')';
			parse res)
		|_   ->  (let res = parse_ident() in
				parse res) in
	parse_lambda();;













