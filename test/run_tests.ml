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
]

let _ = run_test_tt_main tests
