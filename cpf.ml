(** 
 * **File:  **  cpf.ml
 * **Author:**  f.brioccia
 * **Date:  **  2025-11
 * **Info:  **  This files contains some implementations and notes on the theory of computation.
                The content does not claim any scientific rigor as it is part of personal study on the subject.
                Any suggestions or contributions are more than welcome.

* **Licence:**  MIT
* **Version:**  1.0 
*)

(* A Pairing function is a function that reversibly maps two natural numbers to a single natural number.
 * Most notable applications are found in set theory: to show that rational numbers have the same cardinality as natural numbers.
 * and also in computability theory, where pairing functions are used to encode any data or data structure into a single natural number.

 * Given two natural numbers x and y, the Cantor pairing function <x, y> is defined as:
   <x, y> = (1/2) * (x + y) * (x + y + 1) + y

 * This function uniquely maps each pair of natural numbers to a single natural number.
 * <,>: ℕ⨯ℕ → ℕ
 
 Cantor Pairing Function - Graphical shape
 Besides the analytic form of the function, its mechanism can be shown in a graphical form using a table.
 Starting from 0 al the natural numbers are placed on each cell of the table following a diagonal direction (↗).
 The x and y indexes are arguments of the function, the value of the function is the cell content at [x,y].
 
 Es.
 <0, 0> = 0
 <0, 3> = 9
 <2, 2> = 12
    
     x\y  0    1    2    3    4  
        +----+----+----+----+----+...
      0 | 0  | 2  | 5  |  9 | 14 |
        +----+----+----+----+----+...
      1 | 1  | 4  | 8  | 13 | .. |
        +----+----+----+----+----+...
      2 | 3  | 7  | 12 | .. | .. |
        +----+----+----+----+----+...
      3 | 6  | 11 | .. | .. | .. |
        +----+----+----+----+----+...
      4 | 10 | .. | .. | .. | .. |
        +----+----+----+----+----+...
        ..........................
 *)
  
  let cantor_pairing_fun x y =
	  let rec z_pairing n count = 
	   if n = 0 then count
	   else z_pairing (n-1) (count+n)
  in z_pairing (x+y) 0 + y;;

print_endline (Printf.sprintf "<0,0>: %d" (cantor_pairing_fun 0 0));;
print_endline (Printf.sprintf "<1,3>: %d" (cantor_pairing_fun 1 3));;
print_endline (Printf.sprintf "<2,3>: %d" (cantor_pairing_fun 2 3));;
print_endline (Printf.sprintf "<10,20>: %d" (cantor_pairing_fun 10 20));;
print_endline ("");;
 
(*  Inverting the Cantor Pairing Function
	
 *	As said the CPF is used to demonstrate that the sets of ℕ⨯ℕ and ℕ have the same cardinality.
 *	It is necessary to show that CPF constitutes a bijection, in other terms the function must be invertible.
 *	The following two functions: right and left are the implementation of the inverse Cantor pairing function;  
 *	right: ℕ → ℕ computes the x of the C. pair
 *	left:  ℕ → ℕ computes the y of the C. pair
	
 *	The two functions can be built on the graphical shape of the CPF.
 *	In general the number n lies on the diagonal that starts from index : x+y
 *	Es. 8 is the result of <1,2> and it lies on the diagonal that starts in <3,0>
 *	Es. 5 is the result of <0,2> and it lies on the diagonal that starts in <2,0>
	
 *	Then the number n | <x,y> = n; it lies on the diagonal z,0 where z = x+y.
 *	The number that lies on the first position on the diagonal is distant by y point from n.
 *	Es. 8 belongs to the diagonal 3,0 where <3,0> is 6, two next steps away from 8.
	
	
	x\y   0    1    2    3    4  				x\y   .    .    y    .    . 
        +----+----+----+----+----+...				+-----+----+----+----+----+...
      0 | 0  | 2  | 5  |  9 | 14 |				.   | .   | .  | .  |  . |  . |	
        +----+----+----+----+----+...				+-----+----+----+----+----+...
      1 | 1  | 4  | 8  | 13 | .. |				x-->| .   | .  | n  |  . |  . |	
        +----+----+----+----+----+...				+-----+----+----+----+----+...
      2 | 3  | 7  | 12 | .. | .. |				.   | .   | ↗  | .  |  . |  . |
        +----+----+----+----+----+...				+-----+----+----+----+----+...
      3 | 6  | 11 | .. | .. | .. |		      x+y-->|<z,0>| .  | .  |  . |  . |
        +----+----+----+----+----+...				+-----+----+----+----+----+...
      4 | 10 | .. | .. | .. | .. |				↗   | .   | .  | .  |  . |  . |
        +----+----+----+----+----+...				+-----+----+----+----+----+...
        ..........................
	
 * Summing up, the pair <x,y> = n, is equal to <x+y,0> + y = n
 * rearranging for y: y = n-<z,0>. where z is x+y 
 * es. starting from 8 e and moving towards 6 by two steps since on the diagonal numbers differ by +1 or -1 per step.
 * 	   subtracting <z,0> we get the number of steps between the two therefore: y
 * it follows that x = z - y, since z is defined as sum of the two x and y
 
 * The given inverse interpretation of the graphical CPF shape focuses on the problem of finding the origin of the diagonal where n lies.
 * Once obtained the z,0 point, the x and y pair is easily derived.
  
 * The core of the solution is to be able to generate the values of the first column, knowing that
 * the first <z_k,0> > n means that n do not lies on that diagonal or on further ones, but n lies exactly on the previous z_k-1
 * in other terms z is the max z for which <z,0> <= n is true
 
 * Any value on the first column can be computed easily,
 * since is the cumulative sum of the index x
 * <z,0> = SUM from i to z that can also be expressed as n * (n+1) * (1/2)
*)
 
 (** [left n] computes the x component of the Cantor Pairing Function on [n]
	 
	 @param n The non-negative integer for which compute the x term of the CPF.
	 @return The value of $n!$.
	 @raise Invalid_argument if [n] is negative.
	 
	 The algorithm starts with a initial index and accumulator both set to zero.
	 It iterates incrementing the index and the accumulator: $index':= index+1$ $acc':= acc+index$
	 until the value of acc is greater than n, at that point the function returns: $z-y => z-(n-<z,0>)$
	 where i-1 is z, i.e. the index x of the diagonal where n lies
	 acc-i is <z,0> i.e. the value of the CPF on the z,0 position.
	 
	 Example:
	 [left 3] is 0
	 [left 14] is 4	 
 *)
let left n = 
    let rec left_aux n i acc = 
     if acc > n then (i-1)-(n-(acc-i))
     else left_aux n (i+1) (acc+i)
in if n >= 0 then left_aux n 0 0 else raise (Invalid_argument "Undefined");;

print_endline (Printf.sprintf "left(1): %d" (left 1));;
print_endline (Printf.sprintf "left(4): %d" (left 4));;
print_endline (Printf.sprintf "left(5): %d" (left 5));;
print_endline (Printf.sprintf "left(12): %d" (left 12));;

 
 (** [right n] computes the y component of the Cantor Pairing Function on [n]
	 
	 @param n The non-negative integer for which compute the x term of the CPF.
	 @return The value of $n!$.
	 @raise Invalid_argument if [n] is negative.
	 
	 The algorithm starts with a initial index and accumulator both set to zero.
	 It iterates incrementing the index and the accumulator: $index':= index+1$ $acc':= acc+i$
	 until the value of acc is greater than n, at that point the function returns: $n-<z,0>#
	 where (acc-i-1) is <z,0> i.e. the value of the CPF on the z,0 position.
	 
	 Example:
	 [right 3] is 2
	 [right 14] is 0	 
 *)

let right n = 
    let rec right_aux n i acc = 
     if acc > n then n - (acc-(i-1))
     else right_aux n (i+1) (acc+i)
in if n >= 0 then right_aux n 0 0 else raise (Invalid_argument "Undefined");;

print_endline (Printf.sprintf "right(1): %d" (right 1));;
print_endline (Printf.sprintf "right(4): %d" (right 4));;
print_endline (Printf.sprintf "right(5): %d" (right 5));;
print_endline (Printf.sprintf "right(12): %d" (right 12));;

print_endline (Printf.sprintf "<%d,%d>: 15" (left 15) (right 15));;
print_endline (Printf.sprintf "<%d,%d>: 7" (left 7) (right 7));;
print_endline (Printf.sprintf "<%d,%d>: 486" (left 486) (right 486));;

(* 
 *  In conclusion: as shown the cantor pairing function is a bijective function between couples ℕ⨯ℕ and ℕ.
 *  Proving that the two sets have the same cardinality.
 *  Such set theory result has important implications on computability theory,
 *  by providing a fundamental tool to map any kind of data into a single natural number.
 *  All the given fucntions relate to the graphical table construction of the pairing function.
 *  For the sake of completeness the CPF can be described also in a formal analytical manner as follows:
 
 *	<x, y> = (1/2) * (x + y) * (x + y + 1) + y
 *  z = floor(-1+SQRT(8n-7)*(1/2))
 *  right(n)=y= n-<z,0> left(n)=x=z-y
 *)


