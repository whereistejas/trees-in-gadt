open Base
module AVL = Gadt.AVL_for_testing

let cmp a b : Ordering.t = Ordering.of_int (compare a b)

(* QCheck-found failing case *)
let ops =
  [
    `Insert 58;
    `Insert 124;
    `Insert 60;
    `Insert 15;
    `Remove 118;
    `Insert 14;
    `Insert 74;
    `Remove 11;
  ]

let () =
  let tree =
    List.fold ops ~init:AVL.Empty ~f:(fun tree op ->
        let tree' =
          match op with
          | `Insert x ->
              Stdio.printf "Insert %d\n" x;
              AVL.insert x ~cmp tree
          | `Remove x ->
              Stdio.printf "Remove %d\n" x;
              let t, _ = AVL.remove x ~cmp tree in
              t
        in
        Stdio.print_endline (AVL.pp_tree Int.to_string tree');
        Stdio.printf "Height: %d, Skew: %d, Leaning: %s\n" (AVL.height tree')
          (AVL.skew tree')
          (AVL.pp_leaning (AVL.leaning tree'));
        Stdio.print_endline "---";
        tree'
    )
  in
  Stdio.printf "Check invariants: %b\n" (AVL.check_invariants tree ~cmp);
  ignore (AVL.check_and_report tree ~cmp ~show:Int.to_string)
