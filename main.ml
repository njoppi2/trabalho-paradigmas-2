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

let m = Array.init 9 (fun _ -> input_line stdin)
 
*)

let m = [|
"001005300";
"050490000";
"000102064";
"000000750";
"600000001";
"035000000";
"460903000";
"000024090";
"003600100";
|];;


(* In functional programming languages, loops over data structures can be conveniently factored out into higher-order functions, such as the built-in Array.iter function used by our print function: *)

let print() = Array.iter print_endline m
(* The test for validity is performed by looping (using recursion) over i=0..8 and testing the row, column and 3x3 square containing the given coordinate: *)

let rec invalid ?(i=0) x y n =
  i<9 && (m.(y).[i] = n || m.(i).[x] = n ||
      m.(y/3*3 + i/3).[x/3*3 + i mod 3] = n || invalid ~i:(i+1) x y n)
(* Loops over a semi-inclusive range of consecutive integers [l..u) are factored out into a higher-order fold function: *)

let rec fold f accu l u = if l=u then accu else fold f (f accu l) (l+1) u
(* The search function simply examines each position on the board in turn, trying the number 1..9 in each unfilled position: *)

let rec search ?(x=0) ?(y=0) f accu = match x, y with
    9, y -> search ~x:0 ~y:(y+1) f accu (* Next row *)
  | 0, 9 -> f accu                      (* Found a solution *)
  | x, y ->
      if m.(y).[x] <> '0' then search ~x:(x+1) ~y f accu else
        fold (fun accu n ->
                let n = Char.chr (n + 48) in
                if invalid x y n then accu else
                  (m.(y).[x] <- n;
                   let accu = search ~x:(x+1) ~y f accu in
                   m.(y).[x] <- '0';
                   accu)) accu 1 10
(* Note that this search function is itself a higher-order fold, accumulating the value accu by applying the given function f to it whenever a solution m is found. *)

(* The main part of the program uses the search function to accumulate the total number of solutions: *)

let () = Printf.printf "%d solutions\n" (search (fun i -> print(); i+1) 0)

