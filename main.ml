(* open Printf
class board =
    object (self)
      val mutable the_list = ( [] : int list ) (* instance variable *)
      method push x =                        (* push method *)
        the_list <- x :: the_list
      method pop =                           (* pop method *)
        let result = List.hd the_list in
        the_list <- List.tl the_list;
        result
      method peek =                          (* peek method *)
        List.hd the_list
      method size =                          (* size method *)
        List.length the_list
    end;;

let s = new board;;
s#push 2;;
s#push 22;;
s#push 4;;

Printf.printf "Popped %d off the stack.\n" s#peek
 *)










(*
 let initialRenban = """
001005300
050490000
000102064
000000750
600000001
035000000
460903000
000024090
003600100
""";;

let a = input_line stdin;;
Printf.printf "Popped %s off the stack.\n" a;;

let matriz = Array.init 6 (fun _ -> input_line stdin)

*)


let matriz = [|
"000000";
"006050";
"300100";
"020030";
"600000";
"000506";
|];;

let groups = [
  [(0, 0); (0, 1); (1, 1); (2, 1); (2, 2)];
  [(1, 0)];
  [(2, 0); (3, 0); (3, 1); (4, 1)];
  [(4, 0)];
  [(5, 0); (5, 1)];
  [(0, 2); (0, 3); (1, 3)];
  [(1, 2)];
  [(3, 2)];
  [(4, 2); (5, 2)];
  [(2, 3); (3, 3); (4, 3); (5, 3)];
  [(0, 4); (1, 4); (1, 5)];
  [(2, 4)];
  [(3, 4); (2, 5); (3, 5)];
  [(4, 4); (5, 4); (4, 5)];
  [(0, 5)];
  [(5, 5)]
];;

let rec get_nth mylist index =
  match mylist with
    | [] -> raise (Failure "empty list")
    | first::rest -> 
        if index = 0 then first 
        else get_nth rest (index-1);;

let rec find (x: 'a) (list: 'a list): int =
    match list with
      | [] -> -1
      | h :: t -> if x = h then 0 else 1 + find x t

let rec getGroupArray x y list = 
  match list with
  | [] -> -1
  | h :: t ->
    let indexInSubArray = find (x,y) list in
      if indexInSubArray > 0 then indexInSubArray else getGroupArray x y t


let int_to_char i = Char.chr (i + 48);;

(* In functional programming languages, loops over data structures can be conveniently factored out into higher-order functions, such as the built-in Array.iter function used by our print function: *)
let printMatriz() = Array.iter print_endline matriz

(* The test for validity is performed by looping (using recursion) over i=0..8 and testing the row, column and 3x3 square containing the given coordinate: *)
(* this finishes because once one thing return true in the ||'s, the function stops and returns true *)
let rec invalid ?(i=0) x y currentChar =
  i<6
  && (matriz.(y).[i] = currentChar
  || matriz.(i).[x] = currentChar
  (* || matriz.(y/3*3 + i/3).[x/3*3 + i mod 3] = currentChar *)
  || invalid ~i:(i+1) x y currentChar);;

(* Loops over a semi-inclusive range of consecutive integers [l..u) are factored out into a higher-order fold function: *)
let rec fold f accu l u = if l=u then accu else fold f (f accu l) (l+1) u

(* The search function simply examines each position on the board in turn, trying the number 1..6 in each unfilled position: *)
let rec search ?(x=0) ?(y=0) f accu = match x, y with
    6, y -> search ~x:0 ~y:(y+1) f accu (* Next row *)
  | 0, 6 -> f accu                      (* Found a solution *)
  | x, y ->
  (* if the current char is different than 0, search the next char (x + 1) *)
      if matriz.(y).[x] <> '0' then search ~x:(x+1) ~y f accu else
        fold
          (fun accu currentNumber ->
            let currentChar =  int_to_char currentNumber in
            if invalid x y currentChar
              then accu
              else
                (matriz.(y).[x] <- currentChar;
                  let accu = search ~x:(x+1) ~y f accu in
                  matriz.(y).[x] <- '0';
                  accu))
          accu 1 7
(* Note that this search function is itself a higher-order fold, accumulating the value accu by applying the given function f to it whenever a solution matriz is found. *)

(* The main part of the program uses the search function to accumulate the total number of solutions: *)

let () = Printf.printf "%d solutions\n" (search (fun i -> printMatriz(); i+1) 0)
let () = Printf.printf "%s solutions\n" (find (1, 5) (get_nth groups 2))

