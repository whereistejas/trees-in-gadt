(** QCheck property-based tests for AVL tree.

    Similar to Rust's proptest, QCheck generates random inputs and automatically
    shrinks failing cases to minimal reproductions.

    {2 Running the tests}

    {[
      # Build and run
      dune test

      # Or run directly with options
      dune exec test/test_avl_qcheck.exe

      # Run with a specific seed (for reproducibility)
      dune exec test/test_avl_qcheck.exe -- --seed 12345

      # Run more iterations
      dune exec test/test_avl_qcheck.exe -- -v --count 50000
    ]}

    {2 On failure}

    QCheck will print the minimal failing input, e.g.:
    {[
      Test AVL tree maintains invariants failed (5 shrink steps):
      [Insert 58; Insert 124; Insert 60; Insert 15; Remove 118]
    ]}

    Copy this sequence to [test/debug_crash.ml] for detailed investigation. *)

open Base
module AVL = Gadt.AVL_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

(** Generator for insert/remove operations *)
let op_gen : [ `Insert of int | `Remove of int ] QCheck.Gen.t =
  let open QCheck.Gen in
  let* is_remove = bool in
  let* value = int_range 0 127 in
  return (if is_remove then `Remove value else `Insert value)

(** Apply a list of operations to an AVL tree *)
let apply_ops ops =
  List.fold ops ~init:AVL.Empty ~f:(fun tree op ->
      match op with
      | `Insert x -> AVL.insert x ~cmp tree
      | `Remove x -> fst (AVL.remove x ~cmp tree)
  )

(** Apply a list of operations to Base.Avltree (reference implementation) *)
let apply_ops_ref ops =
  List.fold ops ~init:Avltree.empty ~f:(fun tree op ->
      match op with
      | `Insert x ->
          Avltree.add tree ~replace:false ~compare:Int.compare
            ~added:(ref false) ~key:x ~data:()
      | `Remove x ->
          Avltree.remove tree ~removed:(ref false) ~compare:Int.compare x
  )

(** Convert AVL tree to sorted list *)
let avl_to_list tree = AVL.to_list tree

(** Convert Base.Avltree to sorted list *)
let avltree_to_list tree =
  let acc = ref [] in
  Avltree.iter tree ~f:(fun ~key ~data:_ -> acc := key :: !acc);
  List.rev !acc

(** Arbitrary for operations list with shrinking *)
let ops_arb =
  QCheck.make
    ~print:
      QCheck.Print.(
        list (fun op ->
            match op with
            | `Insert x -> Printf.sprintf "Insert %d" x
            | `Remove x -> Printf.sprintf "Remove %d" x
        )
      )
    ~shrink:QCheck.Shrink.list
    QCheck.Gen.(list_size (int_range 1 100) op_gen)

(* === Property Tests === *)
[@@@warning "-32"]

let test_invariants =
  QCheck.Test.make ~count:10000 ~name:"AVL tree maintains invariants" ops_arb
    (fun ops ->
      let tree = apply_ops ops in
      AVL.check_and_report tree ~cmp ~show:Int.to_string
  )

let test_contents_match_reference =
  QCheck.Test.make ~count:10000 ~name:"AVL tree contents match reference"
    ops_arb (fun ops ->
      let tree = apply_ops ops in
      let ref_tree = apply_ops_ref ops in
      let tree_list = avl_to_list tree in
      let ref_list = avltree_to_list ref_tree in
      List.equal Int.equal tree_list ref_list
  )

let test_sorted_output =
  QCheck.Test.make ~count:10000 ~name:"AVL tree to_list is sorted" ops_arb
    (fun ops ->
      let tree = apply_ops ops in
      let tree_list = avl_to_list tree in
      List.is_sorted tree_list ~compare:Int.compare
  )

let test_insert_then_member =
  QCheck.Test.make ~count:5000 ~name:"insert x then x is in tree"
    QCheck.(
      pair
        (make Gen.(list_size (int_range 0 50) (int_range 0 127)))
        (make Gen.(int_range 0 127))
    )
    (fun (ops, x) ->
      let initial_ops = List.map ops ~f:(fun v -> `Insert v) in
      let tree = apply_ops initial_ops in
      let tree_with_x = AVL.insert x ~cmp tree in
      List.mem (avl_to_list tree_with_x) x ~equal:Int.equal
    )

let test_remove_then_not_member =
  QCheck.Test.make ~count:5000 ~name:"remove x then x is not in tree"
    QCheck.(
      pair
        (make Gen.(list_size (int_range 1 50) (int_range 0 127)))
        (make Gen.(int_range 0 127))
    )
    (fun (ops, x) ->
      let initial_ops = List.map ops ~f:(fun v -> `Insert v) in
      let tree = apply_ops initial_ops in
      let tree_without_x, _ = AVL.remove x ~cmp tree in
      not (List.mem (avl_to_list tree_without_x) x ~equal:Int.equal)
    )

(* === Run Tests === *)

(* let _ =
   let open QCheck_runner in
  let tests =
    [
      test_invariants;
      test_contents_match_reference;
      test_sorted_output;
      test_insert_then_member;
      test_remove_then_not_member;
    ]
  in
  run_tests_main tests *)
