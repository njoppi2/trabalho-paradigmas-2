print_int 4;;
let myAdder (a: int) (b: int): int =
  a + b;;

(* print_int(myAdder 2 6);; *)

let isZero (x: int): string =
  match x with
    | 0 -> "true"
    | _ -> "false";;

 (* print_string(isZero 1) *)

 let isListEmpty (l: int list): string = 
  match l with
    | [] -> "true"
    | h::t -> "false";;

(* print_string(isListEmpty [1]);; *)

let myList: int list = [1; 2; 2];;

let rec sumOfList (l: int list): int = 
  begin match l with
    | [] -> 0
    | h::t -> h + (sumOfList t)
  end;;

(* print_int (sumOfList myList) *)

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;

(* print_string(last [ "a" ; "b" ; "c" ; "d" ]);; *)

let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k-1) t;;

print_string(at 4 ["a"; "j"])

  