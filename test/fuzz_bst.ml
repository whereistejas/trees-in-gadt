open Base
module BST = Gadt.BST_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

(** AFL-native fuzz test using OCaml's built-in AFL support.

    Setup:
    {[
      # Create an AFL-enabled switch (one-time)
      opam switch create 5.2.0+afl ocaml-variants.5.2.0+options ocaml-option-afl
      eval $(opam env)
      opam install afl-persistent base

      # Build and run
      dune build test/fuzz_bst.exe
      mkdir -p _fuzz/input _fuzz/output
      echo -e "\x05\x01\x03\x02\x04" > _fuzz/input/seed
      afl-fuzz -i _fuzz/input -o _fuzz/output ./_build/default/test/fuzz_bst.exe
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

(** Apply operations and check invariants *)
let run_test () =
  let ops = parse_ops () in
  let tree, reference =
    List.fold ops
      ~init:(BST.Empty, Set.empty (module Int))
      ~f:(fun (tree, set) op ->
        match op with
        | `Insert x -> (BST.insert x ~cmp tree, Set.add set x)
        | `Remove x ->
            let tree', _ = BST.remove x ~cmp tree in
            (tree', Set.remove set x)
      )
  in
  (* Check invariants *)
  if not (BST.check_invariants tree ~cmp) then failwith "Invariant violation!";
  (* Check contents match *)
  let tree_list = BST.to_list tree in
  let ref_list = Set.to_list reference in
  if not (List.equal Int.equal tree_list ref_list) then
    failwith "Contents mismatch!";
  (* Check sorted *)
  if not (List.is_sorted tree_list ~compare:Int.compare) then
    failwith "Not sorted!"

let () = AflPersistent.run run_test
