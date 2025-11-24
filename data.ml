(* 
 * The Cantor Pairing Function can be use as a tool to encode any given data structure into a single natural number.
 * The following implementation shows how a list of integers can be encoded by a single natural number.
 * L = [x_1,x_2...,x_n] => CPF(x_1, CPF(x_2, CPF(...CPF(x_m,0)))) = n
*)
let list_encode l = 
let rec list_encode_aux current_list acc = match current_list with
	[]-> acc
	|x::xs-> list_encode_aux (xs) (cantor_pairing_fun x acc)
	 in list_encode_aux (List.rev l) 0;;

print_endline (Printf.sprintf "encode [0;1;1;2;3]=%d" (list_encode [0;1;1;2;3]));;


let rec list_decode n = 
	let right_el = right n
	in 
	 if right_el = 0 then [left n]
	 else (left n)::list_decode (right n);;
		
print_endline ("list_decode 295 =[" ^ (List.fold_left (fun acc x-> acc^x^";") "" (List.map string_of_int (list_decode 295)))^"]");;


(** Exercices - More functions **)
(*  1. Lenght function: given an encoded list return the length of the list, -1 otherwise *)
(*	2. K-th projection: given an encoded list and a k index return the k-th element of the list, -1 otherwise *)
(*  3. Increase function: given an encoded list and a k index, return the list where the k-th element is increased by 1 *)
(*  4. Decrease function: given an encoded list and a k index, return the list where the k-th element is decreased by 1 *)

let rec len n = 
	if (right n) = 0
		then 1 else len(right n) +1;;

let kth n k = 
	let rec kth_aux enc k index =
	if k = index 
		then left enc
		else kth_aux (right enc) k (index+1)
	in if  k> (len n)-1 then -1 else kth_aux n k 0;;

let incr n k =
	let rec incr_aux enc k index = 
		let right_el = right enc in
		let left_el = left enc in
		if right_el = 0 
			then [left_el + if k = index then 1 else 0]
			else [left_el + if k = index then 1 else 0] @ (incr_aux (right enc) k (index+1))
	in if  k > (len n)-1 then list_decode n else incr_aux n k 0;;

let decr n k =
	let rec decr_aux enc k index = 
		let right_el = right enc in
		let left_el = left enc in
		if right_el = 0 
			then [left_el - if left_el>0 && k = index then 1 else 0]
			else [left_el - if left_el>0 && k = index then 1 else 0] @ (decr_aux (right enc) k (index+1))
	in if  k > (len n)-1 then list_decode n else decr_aux n k 0;;
