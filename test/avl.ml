open Base
open Base.Poly
module AVL = Gadt.AVL

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

let gen_seq_tree n =
  let seq = List.init n ~f:(fun i -> i + 1) in
  List.fold seq ~init:AVL.Empty ~f:(fun acc x -> AVL.insert x ~cmp acc)

let%expect_test "insert maintains BST and balance invariants" =
  (* Insert elements in an order that would cause imbalance in a regular BST *)
  let tree = gen_seq_tree 7 in
  Stdio.print_string (AVL.pp_tree Int.to_string tree);
  [%expect
    {|
                 4(h=3)
           ┌────────┴────────┐
        2(h=2)            6(h=2)
       ┌───┴────┐      ┌───┴────┐
    1(h=1)   3(h=1)   5(h=1)   7(h=1)
    |}]
(* let%test_unit "assert height of tree is correct" = let tree = gen_seq_tree 6
   in Stdio.print_string (AVL.pp_tree Int.to_string tree); [%test_eq: int]
   (AVL.height tree) 3 *)
