open Base

module BST : sig
  type 'a node =
    | Leaf of 'a
    | Node of {
        value : 'a;
        left : 'a node;
        right : 'a node;
      }
  [@@deriving sexp_of]

  type 'a tree = { node : 'a node } [@@deriving sexp_of]

  val insert : 'a tree -> 'a -> 'a tree
  val pp_tree : ('a -> string) -> 'a tree -> string
end = struct
  type 'a node =
    | Leaf of 'a
    | Node of {
        value : 'a;
        left : 'a node;
        right : 'a node;
      }
  [@@deriving sexp_of]

  type 'a tree = { node : 'a node } [@@deriving sexp_of]

  let insert (_bst : 'a tree) (_value : 'a) : 'a tree = failwith "TODO"

  let pp_tree (show : 'a -> string) (tree : 'a tree) : string =
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
      | Leaf v ->
          let s = show v in
          let width = String.length s in
          { lines = [ s ]; width; root_pos = width / 2 }
      | Node { value; left; right } ->
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
          let val_str = show value in
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
                String.concat [ l; spaces gap; r ])
          in
          {
            lines = value_line :: branch_line :: child_lines;
            width = total_w;
            root_pos;
          }
    in
    build tree.node |> fun { lines; _ } -> String.concat_lines lines
end
