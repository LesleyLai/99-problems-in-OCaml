open OUnit2
open ListProblems

let tests = "test suite for last" >::: [
  "empty"  >:: (fun _ -> assert_equal None (last []));
  "one"    >:: (fun _ -> assert_equal (Some "d") (last ["a" ; "b" ; "c" ; "d"]));
]

let _ = run_test_tt_main tests
