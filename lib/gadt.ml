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
        invalid; use [Leaf elt] instead
      + {b Sorted traversal}: In-order traversal yields sorted list

      {2 Operations}

      - [insert]: Adds value, no-op if present. O(h).
      - [remove]: Removes value. Uses in-order successor. O(h).
      - [member]: Checks existence. O(h).

      {2 Limitations}

      - Not self-balancing: O(n) worst case
      - Uses polymorphic comparison ([Base.Poly]) *)
  type 'a node =
    | Empty
    | Leaf of 'a
    | Node of {
        elt : 'a;
        left : 'a node;
        right : 'a node;
      }

  let compare a b : Ordering.t = Ordering.of_int (compare a b)

  (* TODO: Implement balancing. *)
  let rec insert (node : 'a node) (item : 'a) : 'a node =
    match node with
    | Empty -> Leaf item
    | Leaf elt -> (
        match compare item elt with
        | Ordering.Less -> Node { elt; left = Leaf item; right = Empty }
        | Ordering.Greater -> Node { elt; left = Empty; right = Leaf item }
        | Ordering.Equal -> node
      )
    | Node { elt; left; right } -> (
        match compare item elt with
        | Ordering.Less -> Node { elt; left = insert left item; right }
        | Ordering.Greater -> Node { elt; left; right = insert right item }
        | Ordering.Equal -> node
      )

  type reordering =
    | Succ
    | Pred

  (* TODO: Implement balancing. *)
  let rec remove node item : 'a node * 'a option =
    match node with
    | Empty -> (node, None)
    | Leaf elt -> (
        match compare item elt with
        | Ordering.Less | Ordering.Greater -> (node, None)
        | Ordering.Equal -> (Empty, Some elt)
      )
    | Node { elt; left; right } -> (
        match compare item elt with
        | Ordering.Less -> (
            match remove left item with
            | _, None -> (node, None)
            | Empty, removed when right = Empty -> (Leaf elt, removed)
            | _left, removed -> (Node { elt; left = Empty; right }, removed)
          )
        | Ordering.Greater -> (
            match remove right item with
            | _, None -> (node, None)
            | Empty, removed when left = Empty -> (Leaf elt, removed)
            | _right, removed -> (Node { elt; left; right = Empty }, removed)
          )
        | Ordering.Equal ->
            (* FIXME: Should this be `Empty`? *)
            (Empty, Some elt)
      )

  let rec member node item : bool =
    match node with
    | Empty -> false
    | Leaf lf -> lf = item
    | Node { elt; left; right } -> (
        match compare item elt with
        | Ordering.Less -> member left item
        | Ordering.Greater -> member right item
        | Ordering.Equal -> true
      )

  (* Thank you Claude for this method. *)
  let pp_tree (show : 'a -> string) (node : 'a node) : string =
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
    let rec build (node : 'a node) : Layout.t =
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
end
