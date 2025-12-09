open Base
module AVL = Gadt.Avl_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

(** AFL-native fuzz test using OCaml's built-in AFL support.

    Setup:
    {[
      # Create an AFL-enabled switch (one-time)
      opam switch create 5.2.0+afl ocaml-variants.5.2.0+options ocaml-option-afl
      eval $(opam env)
      opam install afl-persistent base

      # Build and run
      dune build test/fuzz_avl.exe
      mkdir -p _fuzz/input _fuzz/output
      echo -e "\x05\x01\x03\x02\x04" > _fuzz/input/seed
      afl-fuzz -i _fuzz/input -o _fuzz/output ./_build/default/test/fuzz_avl.exe
    ]} *)

(** Parse bytes from stdin into operations. Format: each byte is an operation
    - High bit (0x80): 0 = Insert, 1 = Remove
    - Low 7 bits: the value (0-127) *)
let parse_ops () : [ `Insert of int | `Remove of int ] list =
  let rec read_all acc =
    match In_channel.(input_char stdin) with
    | None -> List.rev acc
    | Some c ->
        let byte = Char.to_int c in
        let is_remove = byte land 0x80 <> 0 in
        let value = byte land 0x7F in
        let op = if is_remove then `Remove value else `Insert value in
        read_all (op :: acc)
  in
  read_all []

(** Convert Base.Avltree to sorted list *)
let avltree_to_list (ref_tree : (int, unit) Avltree.t) : int list =
  let acc = ref [] in
  Avltree.iter ref_tree ~f:(fun ~key ~data:_ -> acc := key :: !acc);
  List.rev !acc

(** Apply operations and check invariants *)
let run_test () =
  let ops = parse_ops () in
  let tree, reference =
    List.fold ops
      ~init:(AVL.Empty, (Avltree.empty : (int, unit) Avltree.t))
      ~f:(fun (tree, ref_tree) op ->
        match op with
        | `Insert x ->
            let ref_tree' =
              Avltree.add ref_tree ~replace:false ~compare:Int.compare
                ~added:(ref false) ~key:x ~data:()
            in
            (AVL.insert x ~cmp tree, ref_tree')
        | `Remove x ->
            let tree', _ = AVL.remove x ~cmp tree in
            let ref_tree' =
              Avltree.remove ref_tree ~removed:(ref false) ~compare:Int.compare
                x
            in
            (tree', ref_tree')
      )
  in
  (* Check invariants *)
  if not (AVL.check_invariants tree ~cmp) then failwith "Invariant violation!";
  (* Check contents match *)
  let tree_list = AVL.to_list tree in
  let ref_list = avltree_to_list reference in
  if not (List.equal Int.equal tree_list ref_list) then
    failwith "Contents mismatch!";
  (* Check sorted *)
  if not (List.is_sorted tree_list ~compare:Int.compare) then
    failwith "Not sorted!"

let () = AflPersistent.run run_test
