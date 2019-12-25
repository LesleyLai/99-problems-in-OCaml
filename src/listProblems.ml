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
