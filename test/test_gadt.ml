open Base
open Base.Poly
module BST = Gadt.BST

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
  let node, removed = BST.remove node 3 in
  assert (removed = Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      5
    ┌─┴─┐
    1   .
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
  let node, removed = BST.remove node 1 in
  assert (removed = Some 1);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect
    {|
          3
      ┌───┴───┐
      2       5
    ┌─┴─┐   ┌─┴─┐
    0   .   4   6
    |}]

let%expect_test "remove right child node" =
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
  let node, removed = BST.remove node 5 in
  assert (removed = Some 5);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
         3
      ┌──┴──┐
      1     .
    ┌─┴─┐
    0   2
    |}]

let%expect_test "remove from empty tree" =
  let node, removed = BST.remove Empty 42 in
  assert (removed = None);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {| . |}]

let%expect_test "remove single leaf" =
  let node, removed = BST.remove (Leaf 1) 1 in
  assert (removed = Some 1);
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
  let _, removed = BST.remove node 99 in
  assert (removed = None);
  [%expect {||}]

let%expect_test "remove node with both children non-empty" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   5
    |}];
  let node, removed = BST.remove node 3 in
  assert (removed = Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|.|}]

let%expect_test "remove node with only left child" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Empty } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}];
  let node, removed = BST.remove node 3 in
  assert (removed = Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|.|}]

let%expect_test "remove node with only right child" =
  let node = BST.Node { elt = 3; left = Empty; right = Leaf 5 } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    .   5
    |}];
  let node, removed = BST.remove node 3 in
  assert (removed = Some 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|.|}]

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
  let node, removed = BST.remove node 1 in
  assert (removed = Some 1);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    .   5
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
  let node, removed = BST.remove node 5 in
  assert (removed = Some 5);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}]

let%expect_test "node downgrades to leaf when only child removed (left)" =
  let node = BST.Node { elt = 3; left = Leaf 1; right = Empty } in
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|
      3
    ┌─┴─┐
    1   .
    |}];
  let node, removed = BST.remove node 1 in
  assert (removed = Some 1);
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
  let node, removed = BST.remove node 5 in
  assert (removed = Some 5);
  assert (node = BST.Leaf 3);
  Stdio.print_string (BST.pp_tree Int.to_string node);
  [%expect {|3|}]
