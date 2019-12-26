open OUnit2
open ListProblems

let tests = "test suite for list problems" >::: [
  "last empty"  >:: (fun _ -> assert_equal None (last []));
  "last multi"    >:: (fun _ -> assert_equal (Some "d") (last ["a" ; "b" ; "c" ; "d"]));

  "last_two none"    >:: (fun _ -> assert_equal None (last_two []));
  "last_two one"    >:: (fun _ -> assert_equal None (last_two ["a"]));
  "last_two multi"    >:: (fun _ -> assert_equal (Some ("c", "d")) (last_two ["a" ; "b" ; "c" ; "d"]));

  "at empty"  >:: (fun _ -> assert_equal None (at 3 []));
  "at negative"  >:: (fun _ -> assert_equal None (at (-1) ["a"]));
  "at big"  >:: (fun _ -> assert_equal None (at 3 ["a"]));
  "at hit"  >:: (fun _ -> assert_equal (Some "d") (at 3 ["a"; "b"; "c"; "d"; "e" ]));

  "length empty"  >:: (fun _ -> assert_equal 0 (length []));
  "length multi"  >:: (fun _ -> assert_equal 4 (length ["a"; "b"; "c"; "d"]));

  "rev multi"  >:: (fun _ -> assert_equal ["c"; "b"; "a"] (rev ["a"; "b"; "c"]));

  "palindrome empty"  >:: (fun _ -> assert_equal true (is_palindrome []));
  "palindrome yes"  >:: (fun _ -> assert_equal true (is_palindrome ["x" ; "a" ; "m" ; "a" ; "x"]));
  "palindrome no"  >:: (fun _ -> assert_equal false (is_palindrome ["a" ; "b"]));

  "flatten"  >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"]
                      (flatten [ One "a"; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]));
]

let _ = run_test_tt_main tests
