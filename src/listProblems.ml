(*
Problem 1
Write a function
last : 'a list -> 'a option
that returns the last element of a list.
*)
let rec last (lst: 'a list) =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl

(*
Problem 2
Find the last but one (last and penultimate) elements of a list. (easy)
*)
let rec last_two (lst: 'a list) =
  match lst with
  | [] | [_] -> None
  | lst' :: lst :: [] -> Some (lst', lst)
  | _ :: tl -> last_two tl
