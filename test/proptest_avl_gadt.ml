(** Property-based tests for Avl_Gadt (GADT-based AVL tree). *)

open Base
module Avl = Gadt.Avl_Gadt_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

(** Generator for unique insert values (Avl_Gadt throws on duplicates) *)
let inserts_gen : int list QCheck.Gen.t =
  let open QCheck.Gen in
  let* len = int_range 1 100 in
  let* raw = list_size (return len) (int_range 0 127) in
  return (List.dedup_and_sort raw ~compare:Int.compare)

(** Apply inserts to GADT AVL tree *)
let apply_inserts elems =
  List.fold elems ~init:Avl.empty ~f:(fun tree x -> Avl.insert x ~cmp tree)

(** Apply inserts to Base.Avltree (reference implementation) *)
let apply_inserts_ref elems =
  List.fold elems ~init:Avltree.empty ~f:(fun tree x ->
      Avltree.add tree ~replace:false ~compare:Int.compare ~added:(ref false)
        ~key:x ~data:()
  )

(** Convert Base.Avltree to sorted list *)
let avltree_to_list tree =
  let acc = ref [] in
  Avltree.iter tree ~f:(fun ~key ~data:_ -> acc := key :: !acc);
  List.rev !acc

let inserts_arb =
  QCheck.make
    ~print:QCheck.Print.(list int)
    ~shrink:QCheck.Shrink.list inserts_gen

let test_matches_reference =
  QCheck.Test.make ~count:10000 ~name:"Avl_Gadt matches Base.Avltree"
    inserts_arb (fun elems ->
      let gadt_tree = apply_inserts elems in
      let ref_tree = apply_inserts_ref elems in
      let gadt_list = Avl.to_list gadt_tree in
      let ref_list = avltree_to_list ref_tree in
      List.equal Int.equal gadt_list ref_list
  )

let _ = QCheck_runner.run_tests_main [ test_matches_reference ]
