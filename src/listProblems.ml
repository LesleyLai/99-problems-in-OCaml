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

(*
7. Flatten a nested list structure
*)

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list

(* My solution use CPS to achieve tail recursion *)
(* let flatten list =
 *   let rec helper acc = function
 *     | [] -> acc
 *     | One x :: t -> helper (x :: acc) t
 *     | Many l :: t -> helper (helper acc l) t in
 *   List.rev (helper [] list) *)

let flatten list =
  let rec helper_k acc lst k =
    match lst with
    | [] -> k(acc)
    | One x :: t -> helper_k (x :: acc) t k
    | Many l :: t ->
      helper_k acc l (fun v1 ->
          helper_k v1 t (fun v2 -> k(v2))
      ) in
  List.rev (helper_k [] list (fun x -> x))

(*
9. Pack consecutive duplicates of list elements into sublists.
*)
let pack (list: 'a list): 'a list list =
  let rec helper acc current = function
    | [] -> acc
    | [elem] -> (elem::current)::acc
    | elem1 :: ((elem2 :: _) as tail) ->
      let new_current = elem1::current in
      if elem1 = elem2 then helper acc new_current tail
      else helper (new_current::acc) [] tail in
  helper [] [] list |> rev

(*
10. Run-length encoding of a list. (easy)
*)
let runlength_encode (list: 'a list): (int * 'a) list =
  let rec helper acc count = function
    | [] -> acc
    | [elem] -> (count+1, elem)::acc
    | elem1 :: ((elem2 :: _) as tail) ->
      if elem1 = elem2 then helper acc (count+1) tail
      else helper ((count+1, elem1)::acc) 0 tail in
  helper [] 0 list |> rev
