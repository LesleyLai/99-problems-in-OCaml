open OUnit2
open ListProblems

let tests = "test suite for list problems" >::: [
  "last empty"  >:: (fun _ -> assert_equal None (last []));
  "last multi"    >:: (fun _ -> assert_equal (Some "d") (last ["a" ; "b" ; "c" ; "d"]));
  "last_two none"    >:: (fun _ -> assert_equal None (last_two []));
  "last_two one"    >:: (fun _ -> assert_equal None (last_two ["a"]));
  "last_two multi"    >:: (fun _ -> assert_equal (Some ("c", "d")) (last_two ["a" ; "b" ; "c" ; "d"]));
]

let _ = run_test_tt_main tests
