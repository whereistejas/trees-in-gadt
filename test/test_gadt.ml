open Base
open Gadt

let%expect_test "check if pretty-printing works" =
  let open BST in
  let left = Node { value = 1; left = Leaf 0; right = Leaf 2 } in
  let right = Node { value = 5; left = Leaf 4; right = Leaf 6 } in
  let tree = { node = Node { value = 3; left; right } } in
  Stdio.print_string (pp_tree Int.to_string tree);
  [%expect
    {|
          3
      ┌───┴───┐
      1       5
    ┌─┴─┐   ┌─┴─┐
    0   2   4   6
    |}]
