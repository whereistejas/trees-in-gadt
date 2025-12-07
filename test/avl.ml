open Base
open Base.Poly
module AVL = Gadt.AVL_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

let gen_balanced_tree n =
  let seq = List.init n ~f:(fun i -> i + 1) in
  List.fold seq ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)

(* Builds a right-leaning chain without balancing *)
let gen_unbalanced_tree n =
  let rec build = function
    | 0 -> AVL.Empty
    | 1 -> AVL.Leaf 1
    | i -> AVL.Node { elt = i; left = build (i - 1); right = Empty }
  in
  build n

let%expect_test "insert maintains BST and balance invariants" =
  (* Insert elements in an order that would cause imbalance in a regular BST *)
  let tree = gen_balanced_tree 7 in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect
    {|
                 4(h=3)
           ┌────────┴────────┐
        2(h=2)            6(h=2)
       ┌───┴────┐      ┌───┴────┐
    1(h=1)   3(h=1)   5(h=1)   7(h=1)
    |}]

let%test_unit "assert height of balanced tree is correct" =
  let tree = gen_balanced_tree 6 in
  [%test_eq: int] (AVL.height tree) 3

let%test_unit "assert skew of unbalanced tree is correct" =
  let tree = gen_unbalanced_tree 7 in
  [%test_eq: AVL.skew] (AVL.skew tree) (AVL.Left (-6))

(* ============================================================================
   Section 1: Rotation Edge Cases
   ============================================================================ *)

let%expect_test
    "LL imbalance - ascending order [1; 2; 3] triggers right rotation" =
  let tree =
    List.fold [ 1; 2; 3 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    |}]

let%expect_test
    "RR imbalance - descending order [3; 2; 1] triggers left rotation" =
  let tree =
    List.fold [ 3; 2; 1 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    |}]

let%expect_test "LR imbalance - [3; 1; 2] triggers double rotation" =
  let tree =
    List.fold [ 3; 1; 2 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    |}]

let%expect_test "RL imbalance - [1; 3; 2] triggers double rotation" =
  let tree =
    List.fold [ 1; 3; 2 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    |}]

(* ============================================================================
   Section 2: Boundary Cases
   ============================================================================ *)

let%expect_test "empty tree - first insertion" =
  let tree = AVL.insert 42 ~cmp AVL.Empty in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {| 42(h=1) |}]

let%expect_test "single element (Leaf) - insert smaller" =
  let tree = AVL.Leaf 5 in
  let tree = AVL.insert 3 ~cmp tree in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
       5(h=2)
       ┌──┴──┐
    3(h=1)   .
    |}]

let%expect_test "single element (Leaf) - insert larger" =
  let tree = AVL.Leaf 5 in
  let tree = AVL.insert 7 ~cmp tree in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect {|
       7(h=2)
       ┌──┴──┐
    5(h=1)   .
    |}]

let%expect_test "duplicate insertion - should be no-op" =
  let tree =
    List.fold [ 2; 1; 3 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  let tree_with_dup = AVL.insert 2 ~cmp tree in
  (* Trees should be structurally identical *)
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  Stdio.print_endline "---";
  Stdio.print_string (AVL.pp_tree Int.to_string tree_with_dup);
  [%expect
    {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    ---
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)
    |}]

(* ============================================================================
   Section 3: Deep Imbalance Propagation / Large Sequential Insertions
   ============================================================================ *)

let%expect_test "large ascending sequence [1..10] - many rotations" =
  let tree = gen_balanced_tree 10 in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect
    {|
                     4(h=4)
           ┌────────────┴────────────┐
        2(h=2)                    8(h=3)
       ┌───┴────┐          ┌───────┴────────┐
    1(h=1)   3(h=1)       6(h=2)           10(h=2)
                         ┌───┴────┐      ┌──┴──┐
                      5(h=1)   7(h=1)   9(h=1)   .
    |}]

let%expect_test "large descending sequence [10..1] - many rotations" =
  let tree =
    List.fold
      (List.init 10 ~f:(fun i -> 10 - i))
      ~init:AVL.Empty
      ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect
    {|
                          7(h=4)
                 ┌───────────┴────────────┐
              3(h=3)                   9(h=2)
          ┌──────┴──────┐      ┌───┴────┐
       2(h=2)        5(h=2)        8(h=1)   10(h=1)
       ┌──┴──┐      ┌───┴────┐
    1(h=1)   .   4(h=1)   6(h=1)
    |}]

let%test "large descending sequence maintains invariants" =
  let tree =
    List.fold
      (List.init 10 ~f:(fun i -> 10 - i))
      ~init:AVL.Empty
      ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  AVL.check_invariants tree ~cmp

(* ============================================================================
   Section 4: Negative Numbers and Mixed Signs
   ============================================================================ *)

let%expect_test "negative and mixed signs [-3; 0; 3; -1; 1]" =
  let tree =
    List.fold [ -3; 0; 3; -1; 1 ] ~init:AVL.Empty ~f:(fun acc x ->
        AVL.insert x ~cmp acc
    )
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect
    {|
              0(h=3)
          ┌──────┴──────┐
       -1(h=2)       3(h=2)
       ┌──┴───┐      ┌──┴──┐
    -3(h=1)   .   1(h=1)   .
    |}]

let%test "mixed signs maintains invariants" =
  let tree =
    List.fold [ -3; 0; 3; -1; 1 ] ~init:AVL.Empty ~f:(fun acc x ->
        AVL.insert x ~cmp acc
    )
  in
  AVL.check_invariants tree ~cmp

(* ============================================================================
   Section 5: Specific Concerning Patterns
   ============================================================================ *)

let%expect_test "insert [1; 2] - skew should be valid" =
  let tree =
    List.fold [ 1; 2 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  Stdio.printf "\nSkew: %s\n" (AVL.pp_skew (AVL.skew tree));
  [%expect
    {|
       2(h=2)
       ┌──┴──┐
    1(h=1)   .

    Skew: Same
    |}]

let%expect_test "insert [1; 2; 3] - triggers rotation, check structure" =
  let tree =
    List.fold [ 1; 2; 3 ] ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)
  in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  Stdio.printf "\nSkew: %s\n" (AVL.pp_skew (AVL.skew tree));
  [%expect
    {|
        2(h=2)
       ┌───┴────┐
    1(h=1)   3(h=1)

    Skew: Same
    |}]

(* ============================================================================
   Section 6: Verify Invariants Hold for Various Insertion Orders
   ============================================================================ *)

let%test "random-ish order maintains invariants" =
  let tree =
    List.fold [ 5; 2; 8; 1; 4; 7; 9; 3; 6; 10 ] ~init:AVL.Empty ~f:(fun acc x ->
        AVL.insert x ~cmp acc
    )
  in
  AVL.check_invariants tree ~cmp

let%test "alternating high-low maintains invariants" =
  let tree =
    List.fold [ 1; 10; 2; 9; 3; 8; 4; 7; 5; 6 ] ~init:AVL.Empty ~f:(fun acc x ->
        AVL.insert x ~cmp acc
    )
  in
  AVL.check_invariants tree ~cmp
