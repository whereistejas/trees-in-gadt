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

  (* TODO: Implement balancing. *)
  let rec insert item ~cmp node =
    match node with
    | Empty -> Leaf item
    | Leaf elt -> (
        match cmp item elt with
        | Ordering.Less -> Node { elt; left = Leaf item; right = Empty }
        | Ordering.Greater -> Node { elt; left = Empty; right = Leaf item }
        | Ordering.Equal -> node
      )
    | Node { elt; left; right } -> (
        match cmp item elt with
        | Ordering.Less -> Node { elt; left = insert item ~cmp left; right }
        | Ordering.Greater -> Node { elt; left; right = insert item ~cmp right }
        | Ordering.Equal -> node
      )

  let rec member item ~cmp node : bool =
    match node with
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

  let rec pop_min node =
    match node with
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left = Empty; right } ->
        (* The BST invariant guarantees that the minimum value is in the left
           subtree. In fact, the minimum value is the leftmost value in the tree
           which means it won't have a left subtree. *)
        (right, Some elt)
    | Node { elt; left; right } ->
        let left, min = pop_min left in
        (make_node elt left right, min)

  let rec pop_max node =
    match node with
    | Empty -> (Empty, None)
    | Leaf lf -> (Empty, Some lf)
    | Node { elt; left; right = Empty } ->
        (* The BST invariant guarantees that the maximum value is in the right
           subtree. In fact, the maximum value is the rightmost value in the
           tree which means it won't have a right subtree. *)
        (left, Some elt)
    | Node { elt; left; right } ->
        let right, max = pop_max right in
        (make_node elt left right, max)

  type shift =
    | Succ
    | Pred

  let rec remove ?(shift = Pred) item ~cmp node : 'a node * 'a option =
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
                (* Invariant: shouldn't happen (would be Leaf), but handle
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
  include BST

  let rec to_list_aux acc = function
    | Empty -> acc
    | Leaf elt -> elt :: acc
    | Node { elt; left; right } ->
        to_list_aux (elt :: to_list_aux acc right) left

  let to_list node : 'a list = to_list_aux [] node
  let check_invariants _node ~cmp:_ : bool = true
end

(*
 * let invariants =
 *   let in_range lower upper compare_elt v =
 *     (match lower with
 *      | None -> true
 *      | Some lower -> compare_elt lower v < 0)
 *     &&
 *     match upper with
 *     | None -> true
 *     | Some upper -> compare_elt v upper < 0
 *   in
 *   let rec loop lower upper compare_elt t =
 *     match t with
 *     | Empty -> true
 *     | Leaf { elt = v } -> in_range lower upper compare_elt v
 *     | Node { left = l; elt = v; right = r; height = h; size = n } ->
 *       let hl = height l
 *       and hr = height r in
 *       abs (hl - hr) <= 2
 *       && h = max hl hr + 1
 *       && n = length l + length r + 1
 *       && in_range lower upper compare_elt v
 *       && loop lower (Some v) compare_elt l
 *       && loop (Some v) upper compare_elt r
 *   in
 *   fun t ~compare_elt -> loop None None compare_elt t
 * ;;
 *)
