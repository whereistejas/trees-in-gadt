open Base
open Base.Poly
module BST = Gadt.BST_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

let%expect_test "check if pretty-printing works" =
  let left = BST.Node { elt = 1; left = Leaf 0; right = Leaf 2 } in
  let right = BST.Node { elt = 5; left = Leaf 4; right = Leaf 6 } in
  let node = BST.Node { elt = 3; left; right } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
          3
      ┌───┴───┐
      1       5
    ┌─┴─┐   ┌─┴─┐
    0   2   4   6
    |}]

let%expect_test "remove root node" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   5
    |}];
  let node, removed = BST.remove 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      1
    ┌─┴─┐
    .   5
    |}]

let%expect_test "remove left child node" =
  let left = BST.Node { elt = 1; left = Leaf 0; right = Leaf 2 } in
  let right = BST.Node { elt = 5; left = Leaf 4; right = Leaf 6 } in
  let node = BST.Node { elt = 3; left; right } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
          3
      ┌───┴───┐
      1       5
    ┌─┴─┐   ┌─┴─┐
    0   2   4   6
    |}];
  let node, removed = BST.remove 1 ~cmp node in
  [%test_eq: int option] removed (Some 1);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
          3
      ┌───┴───┐
      0       5
    ┌─┴─┐   ┌─┴─┐
    .   2   4   6
    |}]

let%expect_test "remove right child node" =
  let left = BST.Node { elt = 1; left = Leaf 0; right = Leaf 2 } in
  let right = BST.Node { elt = 5; left = Leaf 4; right = Leaf 6 } in
  let node = BST.Node { elt = 3; left; right } in
  let node, removed = BST.remove 5 ~cmp node in
  [%test_eq: int option] removed (Some 5);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
          3
      ┌───┴───┐
      1       4
    ┌─┴─┐   ┌─┴─┐
    0   2   .   6
    |}]

let%expect_test "remove from empty tree" =
  let node, removed = BST.remove 42 ~cmp BST.Empty in
  [%test_eq: int option] removed None;
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {| . |}]

let%expect_test "remove single leaf" =
  let node, removed = BST.remove 1 ~cmp (BST.Leaf 1) in
  [%test_eq: int option] removed (Some 1);
  assert (node = BST.Empty);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|.|}]

let%expect_test "remove non-existent element" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   5
    |}];
  let node, removed = BST.remove 99 ~cmp node in
  [%test_eq: int option] removed None;
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   5
    |}]

let%expect_test "remove node with both children non-empty" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   5
    |}];
  let node, removed = BST.remove 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      1
    ┌─┴─┐
    .   5
    |}]

let%expect_test "remove node with only left child with Predecessor shift" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Empty } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}];
  let node, removed = BST.remove 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {| 1 |}]

let%expect_test "remove node with only right child - both shifts should succeed"
    =
  let node = BST.Node { elt = 3; left = Empty; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    .   5
    |}];
  (* Pred should fall back to right child since left is Empty *)
  let node_pred, removed = BST.remove ~shift:BST.Pred 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node_pred);
  [%expect {| 5 |}];
  (* Succ works directly on the right child *)
  let node_succ, removed = BST.remove ~shift:BST.Succ 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node_succ);
  [%expect {| 5 |}]

let%expect_test "remove node with only left child - both shifts should succeed"
    =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Empty } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}];
  (* Succ should fall back to left child since right is Empty *)
  let node_succ, removed = BST.remove ~shift:BST.Succ 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node_succ);
  [%expect {| 1 |}];
  (* Pred works directly on the left child *)
  let node_pred, removed = BST.remove ~shift:BST.Pred 3 ~cmp node in
  [%test_eq: int option] removed (Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node_pred);
  [%expect {| 1 |}]

let%expect_test "remove node where left child is Empty" =
  let left = BST.Node { elt = 1; left = Empty; right = Leaf 2 } in
  let node = BST.Node { elt = 3; left; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
         3
      ┌──┴──┐
      1     5
    ┌─┴─┐
    .   2
    |}];
  let node, removed = BST.remove ~shift:BST.Succ 1 ~cmp node in
  [%test_eq: int option] removed (Some 1);

  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    2   5
    |}]

let%expect_test "remove node where right child is Empty" =
  let right = BST.Node { elt = 5; left = Leaf 4; right = Empty } in
  let node = BST.Node { elt = 3; left = Leaf 1; right } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
       3
    ┌──┴──┐
    1     5
        ┌─┴─┐
        4   .
    |}];
  let node, removed = BST.remove 5 ~cmp node in
  [%test_eq: int option] removed (Some 5);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   4
    |}]

let%expect_test "node downgrades to leaf when only child removed (left)" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Empty } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}];
  let node, removed = BST.remove 1 ~cmp node in
  [%test_eq: int option] removed (Some 1);
  assert (node = BST.Leaf 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|3|}]

let%expect_test "node downgrades to leaf when only child removed (right)" =
  let node = BST.Node { elt = 3; left = Empty; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    .   5
    |}];
  let node, removed = BST.remove 5 ~cmp node in
  [%test_eq: int option] removed (Some 5);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|3|}]
