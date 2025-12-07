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
