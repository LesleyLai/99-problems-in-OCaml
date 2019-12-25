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

(*
Problem 3
Find the k'th element of a list. (easy)
Note: I changed from the original 1 index to 0 index
*)
let rec at (n: int) (lst: 'a list) =
  if n < 0 then
    None
  else
    match lst with
    | [] -> None
    | hd :: tl ->
      if n = 0 then
        Some hd
      else
        at (n-1) tl

(*
4. Find the number of elements of a list. (easy)
*)
let length (lst: 'a list) =
  let rec helper lst' acc =
    match lst' with
    | [] -> acc
    | _ :: tl -> helper tl (acc + 1)
  in
  helper lst 0

(*
5. Reverse a list. (easy)
*)
let rev (lst: 'a list) =
  let rec helper lst' acc =
    match lst' with
    | [] -> acc
    | hd :: tl -> helper tl (hd :: acc)
  in
  helper lst []

(*
6. Find out whether a list is a palindrome.
*)
let is_palindrome (lst: 'a list) =
  lst = rev lst
