open OUnit2
open ListProblems

let to_encode = ["a";"a";"a";"a";
                 "b";"c";"c";"a";
                 "a";"d";"d";"e";
                 "e";"e";"e"]

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

  "unique"  >:: (fun _ -> assert_equal
                              ["a"; "b"; "c"; "a"; "d"; "e"]
                              (unique
                                 ["a";"a";"a";"a";
                                  "b";"c";"c";"a";
                                  "a";"d";"d";"e";
                                  "e";"e";"e"]));

  "pack"  >:: (fun _ -> assert_equal
                              [["a"; "a"; "a"; "a"];
                               ["b"]; ["c"; "c"];
                               ["a"; "a"]; ["d"; "d"];
                               ["e"; "e"; "e"; "e"]]
                              (pack to_encode));

  "runlength encoding" >:: (fun _ -> assert_equal
                               [(4, "a"); (1, "b"); (2, "c"); (2, "a");
                                (2, "d"); (4, "e")]
                              (runlength_encode to_encode));

  "modified runlength encoding" >:: (fun _ -> assert_equal
                                        [ManyCodes (4, "a");
                                         OneCode "b";
                                         ManyCodes (2, "c");
                                         ManyCodes (2, "a");
                                         ManyCodes (2, "d");
                                         ManyCodes (4, "e")]
                              (runlength_encode' to_encode));

  "decode  runlength encoding" >:: (fun _ -> assert_equal to_encode
                                       ((runlength_encode' to_encode)
                                        |> runlength_decode));

  "duplicate" >:: (fun _ -> assert_equal
                      (duplicate ["a";"b";"c";"c";"d"])
                      ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);


  "replicate" >:: (fun _ -> assert_equal
                      (replicate ["a";"b";"c"] 3)
                      ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);

  "drop" >:: (fun _ -> assert_equal
                 (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3)
                 ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
             )
]

let _ = run_test_tt_main tests
