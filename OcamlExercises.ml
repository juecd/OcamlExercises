(* 
   map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)
let rec map2 f l1 l2 =
  match l1,l2 with
  [],[] -> []
  | x1::xs1, x2::xs2 -> (f x1 x2)::(map2 f xs1 xs2);;



(* 
   rev: 'a list -> 'a list
*)
let rev l =
  List.fold_right (fun x xs ->
  match xs with
  [] -> [x]
  | _ -> xs@[x]) l [];;



(* 
   rev2: 'a list -> 'a list
*)
let rev2 l =
   List.fold_left (fun x xs -> xs::x) [] l;;


(* 
   curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
   uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
*)
let curry f =
   (fun e1 -> fun e2 -> f(e1, e2));;

let uncurry f =
   (fun (e1, e2) -> (f e1) e2);;


(* 
   mapAllPairs: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)
let mapAllPairs f l1 l2 =
   let pairs = List.map(fun x1 ->
                  List.map(fun x2 ->
                     (f x1 x2)) l2) l1 in
   List.flatten(pairs);;



(* Dictionaries *)    

(* 
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b
*) 
let empty1 () : ('a * 'b) list = [];;

let put1 (key : 'a) (value : 'b) (dict : ('a * 'b) list) : ('a * 'b) list =
   match dict with
   [] -> [(key, value)]
   | _ -> (key, value)::dict;;

exception Not_found

let rec get1 (key : 'a) (dict : ('a * 'b) list) : 'b =
   match dict with
   [] -> raise Not_found
   | (k', v')::rest -> if key = k' then v' else (get1 key rest);;

	
(* 
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a, 'b) dict2

let empty2 () : ('a, 'b) dict2 = Empty;;

let put2 (key : 'a) (value : 'b) (dict : ('a, 'b) dict2) : ('a, 'b) dict2 =
   match dict with
   Empty -> Entry(key, value, Empty)
   | _ -> Entry(key, value, dict);;

let rec get2 (key : 'a) (dict : ('a, 'b) dict2) : 'b =
   match dict with
   Empty -> raise Not_found
   | Entry(k', v', rest) -> if key = k' then v' else (get2 key rest);;
	
(* 
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b
*)  

type ('a,'b) dict3 = ('a -> 'b)

let empty3 () : ('a, 'b) dict3 =
   function e1 -> raise Not_found;;

let put3 (key : 'a) (value : 'b) (dict : ('a, 'b) dict3) : ('a, 'b) dict3 = 
   function s ->
      if s == key then value else (dict s);;

let get3 (key : 'a) (dict : ('a, 'b) dict3) : 'b =
   (dict key);;

(* Calculators *)    
  
(* A type for arithmetic expressions *)
  


(* 
   evalAExp: aexp -> float
*)

type op = Plus | Minus | Times | Divide
type aexp = Num of float | BinOp of aexp * op * aexp

let rec evalAExp (exp : aexp) : float =
   match exp with
   Num n -> n
   | BinOp (leftSide, opr, rightSide) ->
      match opr with
      Plus -> (evalAExp leftSide) +. (evalAExp rightSide)
      | Minus -> (evalAExp leftSide) -. (evalAExp rightSide)
      | Times -> (evalAExp leftSide) *. (evalAExp rightSide)
      | Divide -> (evalAExp leftSide) /. (evalAExp rightSide);;


(* A type for stack operations *)	  
	  
type sopn = Push of float | Swap | Calculate of op

(* 
   evalRPN: sopn list -> float
*)

(* returns the last to elements swapped *)
let rec swapper (floatStack : float list) : float list =
   (match floatStack with
      [secondToLast; last] -> [last; secondToLast]
      | f::fs -> f::(swapper fs)
   );;

(* recursively calculates a given expression *)
let rec calculatr (floatStack : float list) (opratr : op) : float list =
   match floatStack with
      [secondToLast; last] ->
         (match opratr with
         Plus -> [secondToLast +. last]
         | Minus -> [secondToLast -. last]
         | Times -> [secondToLast *. last]
         | Divide -> [secondToLast /. last]
         )
      | c::cs -> (c::(calculatr cs opratr));;

(* recursively manipulates the float list based on top of sopn list stack and calls calculatr to calculate the results.
Then grabs the result by reversing the float list and grabbing the first element *)
let rec helper (floatStack : float list) (stackExp : sopn list) : float =
   match stackExp with
   [] -> List.hd (List.rev floatStack)
   | x::xs ->
      (match x with
      Push p -> (helper (floatStack@[p]) xs)
      | Swap -> (helper (swapper floatStack) xs)
      | Calculate c -> (helper (calculatr floatStack c) xs)
      )
   ;;

(* calls the recursive function helper and includes a second parameter to hold the floats *)
let evalRPN (stackExp : sopn list) : float =
   (helper [] stackExp);;

  
(* 
   toRPN: aexp -> sopn list
*)

let rec toRPN (exp : aexp) : sopn list =
   match exp with
   Num n -> [Push n]
   | BinOp (l, opp, r) ->
      (toRPN l)@(toRPN r)@[Calculate opp];;

(* 
   toRPNopt: aexp -> (sopn list * int)
*)

(* recursively inspects the left and right sides of the binary operation and rearranges
the commands to compute the result to decrease the float stack size by computing 
nested binary operations first 

Includes a swap instruction for when the binary operator is non-commutative *)
let rec optHelper (exp : aexp) (minSize : int) : (sopn list * int) =
   match exp with
   Num n -> ([Push n], minSize)
   | BinOp (lft, opr, rght) ->
      (let (leftMinList, leftMinSize) = (optHelper lft (minSize + 1)) in
         (let (rightMinList, rightMinSize) = (optHelper rght (minSize + 1)) in
            (if leftMinSize < rightMinSize then
               (if opr = Minus || opr = Divide then
                  (rightMinList@leftMinList@[Swap]@[Calculate opr], rightMinSize)
               else
                  (rightMinList@leftMinList@[Calculate opr], rightMinSize))
            else
               (leftMinList@rightMinList@[Calculate opr], leftMinSize))
         )
      );;

(* returns a single element sopn list if exp is only one Push (ignoring possibility of 
stack underflow -- also, optHelper does not return the correct minSize in this case), otherwise,
calls the optHelper to recursively calculate the expression with a minimum float stack size *)
let toRPNopt (exp : aexp) : (sopn list * int) =
   match exp with
   Num n -> ([Push n], 1)
   | _ -> (optHelper exp 0);;
   