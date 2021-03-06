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
8. Eliminate consecutive duplicates of list elements. (medium)
*)
let unique (list: 'a list): 'a list =
  let rec helper acc = function
    | elem1 :: ((elem2 :: _) as tail) ->
      if elem1 = elem2 then helper acc tail
      else helper (elem1 :: acc) tail
    | [elem] -> elem::acc
    | [] -> acc in
  helper [] list |> rev


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

(*
11. Modified run-length encoding. (easy)
*)
type 'a code =
    | OneCode of 'a
    | ManyCodes of int * 'a

let runlength_encode' (list: 'a list): 'a code list =
  let from_tuple (count, elem) =
    if count = 1 then OneCode elem
    else ManyCodes (count, elem)
  in
  let rec helper acc count = function
    | [] -> acc
    | [elem] -> (from_tuple(count+1, elem))::acc
    | elem1 :: ((elem2 :: _) as tail) ->
      if elem1 = elem2 then helper acc (count+1) tail
      else helper ((from_tuple(count+1, elem1))::acc) 0 tail in
  helper [] 0 list |> rev

let rec duplicate (elem: 'a) (count: int) (acc: 'a list): 'a list =
  if count = 0 then acc
  else duplicate elem (count - 1) (elem :: acc)

(*
12. Decode a run-length encoded list. (medium)
*)
let runlength_decode (code: 'a code list): 'a list =
  let rec helper acc = function
    | [] -> acc
    | (OneCode x) :: tl -> helper (x :: acc) tl
    | (ManyCodes (n, x)) :: tl -> helper (duplicate x n acc) tl
  in
  helper [] code |> rev

(*
14. Duplicate the elements of a list. (easy)
*)
let duplicate (lst: 'a list): 'a list =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl -> helper (hd :: hd :: acc) tl
  in
  helper [] lst |> rev


(*
15. Replicate the elements of a list a given number of times. (medium)
*)
let rec add_duplicate (lst: 'a list) (count: int) (elem: 'a): 'a list =
  if count = 0 then
    lst
  else
    add_duplicate (elem :: lst) (count - 1) elem

let replicate (lst: 'a list) (count: int): 'a list =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl ->
      helper (add_duplicate acc count hd) tl
  in
  helper [] lst |> rev

(*
16. Drop every N'th element from a list. (medium)
*)
let drop (lst: 'a list) (num: int): 'a list =
  let rec helper acc num' = function
    | [] -> acc
    | hd :: tl ->
      if num' = 1 then
        helper acc num tl
      else
        helper (hd :: acc) (num' - 1) tl
  in
  helper [] num lst |> rev

(*
17. Split a list into two parts; the length of the first part is given. (easy)
*)
let split (lst: 'a list) (num: int): 'a list * 'a list =
  let rec helper acc num' = function
    | [] -> (acc |> rev, [])
    | hd :: tl as lst' ->
      if num' = 0 then
        (acc |> rev, lst')
      else
        helper (hd :: acc) (num' - 1) tl
  in
  helper [] num lst
