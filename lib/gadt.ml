open Base

module BST = struct
  open Base.Poly

  (** Binary Search Tree (unbalanced)

      {2 Structure}
      Three node types:
      - [Empty]: represents absence of a subtree
      - [Leaf]: a value with no children
      - [Node]: a value with left and right subtrees

      {2 Invariants}
      + {b Ordering}: For any [Node { elt; left; right }], all values in [left]
        < [elt] < all values in [right]
      + {b No duplicates}: Each value appears at most once
      + {b Leaf normalization}: [Node { elt; left = Empty; right = Empty }] is
        invalid; use [Leaf elt] instead. Enforced by [make_node].
      + {b Sorted traversal}: In-order traversal yields sorted list
      + {b Minimum is leftmost}: The minimum value has no left child. Exploited
        by [pop_min] for O(h) extraction.
      + {b Maximum is rightmost}: The maximum value has no right child.
        Exploited by [pop_max] for O(h) extraction.

      {2 Operations}
      All operations take a [~cmp] comparator function.
      {ul
       {- [insert]: Adds value, no-op if present. O(h). }
       {- [remove]: Removes value and returns [(new_tree, removed_value)]. O(h).
          Uses [~shift] to choose replacement strategy:
          - [Succ]: Replace with in-order successor (min of right subtree)
          - [Pred]: Replace with in-order predecessor (max of left subtree)
       }
       {- [member]: Checks existence. O(h). }
       {- [pop_min]: Extracts minimum value and returns modified tree. O(h). }
       {- [pop_max]: Extracts maximum value and returns modified tree. O(h). }
       {- [make_node]: Smart constructor that enforces leaf normalization. }
       {- [pp_tree]: Pretty-prints the tree structure. }
      }

      {2 Limitations}
      - Not self-balancing: O(n) worst case for degenerate trees
      - Requires explicit comparator ([~cmp]) for all search operations *)
  type 'a node =
    | Empty
    | Leaf of 'a
    | Node of {
        elt : 'a;
        left : 'a node;
        right : 'a node;
      }

  type shift =
    | Succ
    | Pred

  (* Thank you Claude for this method. *)
  let pp_tree (show : 'a -> string) node : string =
    let module Layout = struct
      type t = {
        lines : string list;
        width : int;
        root_pos : int;
      }
    end in
    let repeat_str s n =
      if n <= 0 then "" else List.init n ~f:(Fn.const s) |> String.concat
    in
    let pad_right ~width s =
      let padding = width - String.length s in
      if padding > 0 then s ^ String.make padding ' ' else s
    in
    let spaces n = String.make n ' ' in
    let make_branch ~left_pos ~root_pos ~right_pos =
      String.concat
        [
          spaces left_pos;
          "┌";
          repeat_str "─" (root_pos - left_pos - 1);
          "┴";
          repeat_str "─" (right_pos - root_pos - 1);
          "┐";
        ]
    in
    let pad_lines ~width ~target_len lines =
      let padded = List.map lines ~f:(pad_right ~width) in
      let extra = target_len - List.length lines in
      if extra > 0 then padded @ List.init extra ~f:(Fn.const (spaces width))
      else padded
    in
    let gap = 3 in
    let rec build node : Layout.t =
      match node with
      | Empty -> { lines = [ "." ]; width = 1; root_pos = 0 }
      | Leaf v ->
          let s = show v in
          let width = String.length s in
          { lines = [ s ]; width; root_pos = width / 2 }
      | Node { elt; left; right } ->
          let Layout.
                { lines = left_lines; width = left_w; root_pos = left_root } =
            build left
          in
          let Layout.
                { lines = right_lines; width = right_w; root_pos = right_root }
              =
            build right
          in
          let total_w = left_w + gap + right_w in
          let right_anchor = left_w + gap + right_root in
          let root_pos = (left_root + right_anchor) / 2 in
          let val_str = show elt in
          let val_start = root_pos - (String.length val_str / 2) in
          let value_line = spaces (max 0 val_start) ^ val_str in
          let branch_line =
            make_branch ~left_pos:left_root ~root_pos ~right_pos:right_anchor
          in
          let max_lines =
            max (List.length left_lines) (List.length right_lines)
          in
          let left_padded =
            pad_lines ~width:left_w ~target_len:max_lines left_lines
          in
          let right_padded =
            pad_lines ~width:right_w ~target_len:max_lines right_lines
          in
          let child_lines =
            List.map2_exn left_padded right_padded ~f:(fun l r ->
                String.concat [ l; spaces gap; r ]
            )
          in
          {
            lines = value_line :: branch_line :: child_lines;
            width = total_w;
            root_pos;
          }
    in
    build node |> fun { lines; _ } -> String.concat_lines lines

  (* FIXME: `insert` does unnecessary allocations in the `Node` branch. *)
  let rec insert item ~cmp = function
    | Empty -> Leaf item
    | Leaf lf -> (
        match cmp item lf with
        | Ordering.Less -> Node { elt = lf; left = Leaf item; right = Empty }
        | Ordering.Greater -> Node { elt = lf; left = Empty; right = Leaf item }
        | Ordering.Equal -> Leaf lf
      )
    | Node { elt; left; right } as node -> (
        match cmp item elt with
        | Ordering.Less -> Node { elt; left = insert item ~cmp left; right }
        | Ordering.Greater -> Node { elt; left; right = insert item ~cmp right }
        | Ordering.Equal -> node
      )

  let rec member item ~cmp = function
    | Empty -> false
    | Leaf lf -> cmp item lf = Ordering.Equal
    | Node { elt; left; right } -> (
        match cmp item elt with
        | Ordering.Less -> member item ~cmp left
        | Ordering.Greater -> member item ~cmp right
        | Ordering.Equal -> true
      )

  let make_node elt left right =
    match (left, right) with
    | Empty, Empty -> Leaf elt
    | _, _ -> Node { elt; left; right }

  let rec pop_min = function
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left = Empty; right } ->
        (* INVARIANTS: The BST invariant guarantees that the minimum value is in
           the left subtree. In fact, the minimum value is the leftmost value in
           the tree which means it won't have a left subtree. *)
        (right, Some elt)
    | Node { elt; left; right } ->
        let left, min = pop_min left in
        (make_node elt left right, min)

  let rec pop_max = function
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left; right = Empty } ->
        (* INVARIANTS: The BST invariant guarantees that the maximum value is in
           the right subtree. In fact, the maximum value is the rightmost value
           in the tree which means it won't have a right subtree. *)
        (left, Some elt)
    | Node { elt; left; right } ->
        let right, max = pop_max right in
        (make_node elt left right, max)

  let rec remove item ~cmp ?(shift = Pred) = function
    | Empty -> (Empty, None)
    | Leaf lf as leaf -> (
        match cmp item lf with
        | Ordering.Equal -> (Empty, Some lf)
        | Ordering.Less | Ordering.Greater -> (leaf, None)
      )
    | Node { elt; left; right } as node -> (
        match cmp item elt with
        | Ordering.Equal -> (
            (* Check subtree availability first, then apply shift preference *)
            match (left, right) with
            | Empty, Empty ->
                (* WARNING: shouldn't happen (would be Leaf), but handle
                   safely *)
                (Empty, Some elt)
            | Empty, right ->
                (* Only right subtree exists - use successor *)
                let right, min = pop_min right in
                (make_node (Option.value_exn min) Empty right, Some elt)
            | left, Empty ->
                (* Only left subtree exists - use predecessor *)
                let left, max = pop_max left in
                (make_node (Option.value_exn max) left Empty, Some elt)
            | left, right -> (
                (* Both subtrees exist - use shift preference *)
                match shift with
                | Succ ->
                    let right, min = pop_min right in
                    (make_node (Option.value_exn min) left right, Some elt)
                | Pred ->
                    let left, max = pop_max left in
                    (make_node (Option.value_exn max) left right, Some elt)
              )
          )
        | Ordering.Less -> (
            let left, removed = remove ~shift item ~cmp left in
            match removed with
            | Some _ -> (make_node elt left right, removed)
            | None -> (node, None)
          )
        | Ordering.Greater -> (
            let right, removed = remove ~shift item ~cmp right in
            match removed with
            | Some _ -> (make_node elt left right, removed)
            | None -> (node, None)
          )
      )
end

module BST_for_testing = struct
  open Base.Poly
  include BST

  let to_list node : 'a list =
    let rec to_list_aux acc = function
      | Empty -> acc
      | Leaf elt -> elt :: acc
      | Node { elt; left; right } ->
          to_list_aux (elt :: to_list_aux acc right) left
    in
    to_list_aux [] node

  (** Check that all BST invariants hold:
      - {b Ordering}: [left] < [elt] < [right]
      - {b No duplicates}: implied by strict ordering
      - {b Leaf normalization}: no [Node] with both children [Empty] *)
  let check_invariants node ~cmp : bool =
    (* Check if elt is within (lower, upper) bounds *)
    let in_range ~lower ~upper elt =
      let lower_ok =
        match lower with None -> true | Some lo -> cmp lo elt = Ordering.Less
      in
      let upper_ok =
        match upper with None -> true | Some hi -> cmp elt hi = Ordering.Less
      in
      lower_ok && upper_ok
    in
    let rec loop ~lower ~upper = function
      | Empty -> true
      | Leaf elt -> in_range ~lower ~upper elt
      | Node { elt; left; right } ->
          (* INVARIANTS: Leaf normalization - shouldn't have both children
             Empty *)
          let normalized = not (left = Empty && right = Empty) in
          (* INVARIANTS: Ordering - elt must be within bounds *)
          let ordered = in_range ~lower ~upper elt in
          (* Recurse: left gets upper bound, right gets lower bound *)
          normalized
          && ordered
          && loop ~lower ~upper:(Some elt) left
          && loop ~lower:(Some elt) ~upper right
    in
    loop ~lower:None ~upper:None node
end

module AVL = struct
  open Base.Poly

  (** A self-balancing BST data structure.

      We defined a balanced tree by calculating the [skew] for that node where,
      [skew = (height right) - (height left)]

      The [height] of a node is the length of the longest downward path from
      this node, counting the number of edges where,
      [height = max (height left) (height right) + 1]

      {2 Invariants}
      + {b Ordering}: For any [Node { elt; left; right }], all values in [left]
        < [elt] < all values in [right]
      + {b Balance}: For any [Node { elt; left; right }], the skew is either
        [-1, 0 , 1].
      + {b Leaf normalization}: [Node { elt; left = Empty; right = Empty }] is
        invalid; use [Leaf elt] instead. Enforced by [make_node].

      `Base` has its own `Avltreee` implementation that we can use for fuzz
      testing. *)
  type 'a node =
    | Empty
    | Leaf of 'a
    | Node of {
        elt : 'a;
        left : 'a node;
        right : 'a node;
      }

  let rec height node =
    (* The order of the patterns is important. *)
    match node with
    | Empty -> 0
    | Leaf _ -> 1
    | Node { elt = _; left; right } -> 1 + max (height left) (height right)

  (* Thank you Claude for this method. *)
  let pp_tree (show : 'a -> string) node : string =
    let module Layout = struct
      type t = {
        lines : string list;
        width : int;
        root_pos : int;
      }
    end in
    let repeat_str s n =
      if n <= 0 then "" else List.init n ~f:(Fn.const s) |> String.concat
    in
    let pad_right ~width s =
      let padding = width - String.length s in
      if padding > 0 then s ^ String.make padding ' ' else s
    in
    let spaces n = String.make n ' ' in
    let make_branch ~left_pos ~root_pos ~right_pos =
      String.concat
        [
          spaces left_pos;
          "┌";
          repeat_str "─" (root_pos - left_pos - 1);
          "┴";
          repeat_str "─" (right_pos - root_pos - 1);
          "┐";
        ]
    in
    let pad_lines ~width ~target_len lines =
      let padded = List.map lines ~f:(pad_right ~width) in
      let extra = target_len - List.length lines in
      if extra > 0 then padded @ List.init extra ~f:(Fn.const (spaces width))
      else padded
    in
    let gap = 3 in
    let rec build node : Layout.t =
      match node with
      | Empty -> { lines = [ "." ]; width = 1; root_pos = 0 }
      | Leaf v ->
          let s = show v ^ "(h=" ^ Int.to_string (height node) ^ ")" in
          let width = String.length s in
          { lines = [ s ]; width; root_pos = width / 2 }
      | Node { elt; left; right } ->
          let Layout.
                { lines = left_lines; width = left_w; root_pos = left_root } =
            build left
          in
          let Layout.
                { lines = right_lines; width = right_w; root_pos = right_root }
              =
            build right
          in
          let total_w = left_w + gap + right_w in
          let right_anchor = left_w + gap + right_root in
          let root_pos = (left_root + right_anchor) / 2 in
          let val_str = show elt ^ "(h=" ^ Int.to_string (height node) ^ ")" in
          let val_start = root_pos - (String.length val_str / 2) in
          let value_line = spaces (max 0 val_start) ^ val_str in
          let branch_line =
            make_branch ~left_pos:left_root ~root_pos ~right_pos:right_anchor
          in
          let max_lines =
            max (List.length left_lines) (List.length right_lines)
          in
          let left_padded =
            pad_lines ~width:left_w ~target_len:max_lines left_lines
          in
          let right_padded =
            pad_lines ~width:right_w ~target_len:max_lines right_lines
          in
          let child_lines =
            List.map2_exn left_padded right_padded ~f:(fun l r ->
                String.concat [ l; spaces gap; r ]
            )
          in
          {
            lines = value_line :: branch_line :: child_lines;
            width = total_w;
            root_pos;
          }
    in
    build node |> fun { lines; _ } -> String.concat_lines lines

  type shift =
    | Succ
    | Pred

  type skew =
    | Same
    | Left of int
    | Right of int
  [@@deriving sexp, compare]

  let pp_skew skew = Sexp.to_string_hum (sexp_of_skew skew)

  let skew node =
    match node with
    | Empty -> Same
    | Leaf _ -> Same
    | Node { elt = _; left; right } ->
        let skew = height right - height left in
        if skew < -1 then Left skew else if skew > 1 then Right skew else Same

  let make_node elt left right =
    if left = Empty && right = Empty then Leaf elt
    else Node { elt; left; right }

  let rec rotate_right elt left_child right_child =
    match left_child with
    | Node { elt = l_elt; left = l_left; right = l_right } ->
        let new_right = make_node elt l_right right_child in
        let balanced_right = balance new_right in
        make_node l_elt l_left balanced_right
    | Empty | Leaf _ -> assert false

  and rotate_left elt left_child right_child =
    match right_child with
    | Node { elt = r_elt; left = r_left; right = r_right } ->
        let new_left = make_node elt left_child r_left in
        let balanced_left = balance new_left in
        make_node r_elt balanced_left r_right
    | Empty | Leaf _ -> assert false

  and balance node =
    match node with
    | Empty -> Empty
    | Leaf _ -> node
    | Node { elt; left = left_child; right = right_child } -> (
        let skew = skew node in
        match skew with
        | Same -> node
        | Left _ -> rotate_right elt left_child right_child
        | Right _ -> rotate_left elt left_child right_child
      )

  let rec insert item ~cmp node =
    ( match node with
      | Empty -> Leaf item
      | Leaf lf -> (
          (* FIXME: The allocation in `Leaf lf` is wasteful, just return the existing leaf. *)
          match cmp item lf with
          | Ordering.Equal -> Leaf lf
          | Ordering.Less -> Node { elt = lf; left = Leaf item; right = Empty }
          | Ordering.Greater ->
              Node { elt = item; left = Leaf lf; right = Empty }
        )
      | Node { elt; left; right } -> (
          match cmp item elt with
          | Ordering.Equal -> node
          | Ordering.Less -> Node { elt; left = insert item ~cmp left; right }
          | Ordering.Greater ->
              Node { elt; left; right = insert item ~cmp right }
        )
      )
    |> balance

  let rec pop_min = function
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left = Empty; right } ->
        (* INVARIANTS: The BST invariant guarantees that the minimum value is in
           the left subtree. In fact, the minimum value is the leftmost value in
           the tree which means it won't have a left subtree. *)
        (right, Some elt)
    | Node { elt; left; right } ->
        let left, min = pop_min left in
        (make_node elt left right, min)

  let rec pop_max = function
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left; right = Empty } ->
        (* INVARIANTS: The BST invariant guarantees that the maximum value is in
           the right subtree. In fact, the maximum value is the rightmost value
           in the tree which means it won't have a right subtree. *)
        (left, Some elt)
    | Node { elt; left; right } ->
        let right, max = pop_max right in
        (make_node elt left right, max)

  let rec remove item ~cmp ?(shift = Pred) node =
    let node, removed =
      match node with
      | Empty -> (Empty, None)
      | Leaf lf as leaf -> (
          match cmp item lf with
          | Ordering.Equal -> (Empty, Some lf)
          | Ordering.Less | Ordering.Greater -> (leaf, None)
        )
      | Node { elt; left; right } as node -> (
          match cmp item elt with
          | Ordering.Equal -> (
              (* Check subtree availability first, then apply shift preference *)
              match (left, right) with
              | Empty, Empty ->
                  (* WARNING: shouldn't happen (would be Leaf), but handle
                   safely *)
                  (Empty, Some elt)
              | Empty, right ->
                  (* Only right subtree exists - use successor *)
                  let right, min = pop_min right in
                  (make_node (Option.value_exn min) Empty right, Some elt)
              | left, Empty ->
                  (* Only left subtree exists - use predecessor *)
                  let left, max = pop_max left in
                  (make_node (Option.value_exn max) left Empty, Some elt)
              | left, right -> (
                  (* Both subtrees exist - use shift preference *)
                  match shift with
                  | Succ ->
                      let right, min = pop_min right in
                      (make_node (Option.value_exn min) left right, Some elt)
                  | Pred ->
                      let left, max = pop_max left in
                      (make_node (Option.value_exn max) left right, Some elt)
                )
            )
          | Ordering.Less -> (
              let left, removed = remove ~shift item ~cmp left in
              match removed with
              | Some _ -> (make_node elt left right, removed)
              | None -> (node, None)
            )
          | Ordering.Greater -> (
              let right, removed = remove ~shift item ~cmp right in
              match removed with
              | Some _ -> (make_node elt left right, removed)
              | None -> (node, None)
            )
        )
    in
    (balance node, removed)

  let rec member item ~cmp = function
    | Empty -> false
    | Leaf lf -> cmp item lf = Ordering.Equal
    | Node { elt; left; right } -> (
        match cmp item elt with
        | Ordering.Less -> member item ~cmp left
        | Ordering.Greater -> member item ~cmp right
        | Ordering.Equal -> true
      )
end

module AVL_for_testing = struct
  open Base.Poly
  include AVL

  let to_list node : 'a list =
    let rec to_list_aux acc = function
      | Empty -> acc
      | Leaf elt -> elt :: acc
      | Node { elt; left; right } ->
          to_list_aux (elt :: to_list_aux acc right) left
    in
    to_list_aux [] node

  type violation =
    | Out_of_range of {
        elt : int;
        lower : int option;
        upper : int option;
      }
    | Unbalanced of {
        elt : int;
        skew : skew;
      }
    | Not_normalized of { elt : int }

  let pp_violation = function
    | Out_of_range { elt; lower; upper } ->
        Printf.sprintf "Out_of_range: elt=%d, lower=%s, upper=%s" elt
          (Option.value_map lower ~default:"None" ~f:Int.to_string)
          (Option.value_map upper ~default:"None" ~f:Int.to_string)
    | Unbalanced { elt; skew } ->
        Printf.sprintf "Unbalanced: elt=%d, skew=%s" elt (pp_skew skew)
    | Not_normalized { elt } ->
        Printf.sprintf "Not_normalized: elt=%d (Node with both children Empty)"
          elt

  (** Check that all AVL invariants hold:
      - {b Ordering}: [left] < [elt] < [right]
      - {b Balance}: |height(left) - height(right)| <= 1 for every node
      - {b No duplicates}: implied by strict ordering
      - {b Leaf normalization}: no [Node] with both children [Empty] Returns
        list of violations (empty if valid). *)
  let find_violations node ~cmp : violation list =
    let in_range ~lower ~upper elt =
      Option.for_all lower ~f:(fun lo -> cmp lo elt = Ordering.Less)
      && Option.for_all upper ~f:(fun hi -> cmp elt hi = Ordering.Less)
    in
    let rec loop ~lower ~upper node =
      match node with
      | Empty -> []
      | Leaf elt ->
          if in_range ~lower ~upper elt then []
          else [ Out_of_range { elt; lower; upper } ]
      | Node { elt; left; right } ->
          let order_violations =
            if in_range ~lower ~upper elt then []
            else [ Out_of_range { elt; lower; upper } ]
          in
          let balance_violations =
            match skew node with
            | Same -> []
            | s -> [ Unbalanced { elt; skew = s } ]
          in
          let normalization_violations =
            if left = Empty && right = Empty then [ Not_normalized { elt } ]
            else []
          in
          order_violations
          @ balance_violations
          @ normalization_violations
          @ loop ~lower ~upper:(Some elt) left
          @ loop ~lower:(Some elt) ~upper right
    in
    loop ~lower:None ~upper:None node

  let check_invariants node ~cmp : bool =
    List.is_empty (find_violations node ~cmp)

  (** Check invariants and print tree + violations if any found. *)
  let check_and_report node ~cmp ~show : bool =
    let violations = find_violations node ~cmp in
    if List.is_empty violations then true
    else (
      Stdio.print_endline "AVL invariant violations found:";
      Stdio.print_endline (pp_tree show node);
      List.iter violations ~f:(fun v ->
          Stdio.print_endline ("  - " ^ pp_violation v)
      );
      false
    )
end
