# BST AVL Balancing and 2-3 Tree Implementations

## Overview

Add AVL balancing to the existing BST, then implement both a regular 2-3 tree and a GADT-enforced 2-3 tree to demonstrate compile-time invariant enforcement.

**Why AVL?** AVL trees track height explicitly at runtime. This creates a conceptual bridge to the GADT 2-3 tree, where height becomes a _type parameter_ instead of a _runtime value_.

## Phase 0: Add Show Notes to README ✓

Added a "Show Notes" section explaining:

- The progression: BST → 2-3 Tree → GADT 2-3 Tree
- Semantic vs syntactic invariants
- Why fuzzing is needed for normal trees but not GADT trees
- The key insight: GADTs make invalid states unrepresentable

## Phase 1: Add AVL Balancing to BST

AVL trees maintain strict balance: |height(left) - height(right)| ≤ 1

**Node type change:**

```ocaml
type 'a node =
  | Empty
  | Leaf of 'a  (* height implicitly 0 *)
  | Node of {
      elt : 'a;
      left : 'a node;
      right : 'a node;
      height : int;
    }
```

**Core operations needed:**

- `height` - returns height of a node (0 for Empty/Leaf)
- `balance_factor` - returns left height - right height
- `rotate_left`, `rotate_right` - single rotations
- `rebalance` - check balance factor and rotate if needed
- Update `insert` to rebalance after insertion
- Update `remove` to rebalance after deletion
- Update `make_node` to compute height

**Key insight:** Height is tracked at runtime; later in GADT version, height moves to the type level.

## Phase 2: Normal 2-3 Tree

Create a new module in `lib/gadt.ml` with standard algebraic types:

```ocaml
type 'a node =
  | Leaf
  | Node2 of 'a node * 'a * 'a node
  | Node3 of 'a node * 'a * 'a node * 'a * 'a node
```

Operations: `insert`, `member`, `remove`, `check_invariants` (for fuzzing).

Invariants to test at runtime:

- All leaves at same depth (balance)
- Ordering: left < elt < right

## Phase 3: GADT 2-3 Tree

Encode height in the type system:

```ocaml
type zero
type 'n succ

type ('a, 'n) tree =
  | Leaf : ('a, zero) tree
  | Node2 : ('a, 'n) tree * 'a * ('a, 'n) tree -> ('a, 'n succ) tree
  | Node3 : ('a, 'n) tree * 'a * ('a, 'n) tree * 'a * ('a, 'n) tree -> ('a, 'n succ) tree

type 'a some_tree = Tree : ('a, 'n) tree -> 'a some_tree
```

The balance invariant is now **compile-time enforced** — no `check_invariants` needed for balance.

## Demonstration

Show that the fuzzer in `test/fuzz_afl.ml` can catch balance bugs in the normal 2-3 tree, but the GADT version cannot even express an unbalanced tree.

---

## Progress

- [x] Add Show Notes section to README
- [ ] Add AVL balancing to BST (height tracking + rotations)
- [ ] Implement normal 2-3 tree module with runtime invariant checking
- [ ] Implement GADT 2-3 tree with compile-time balance enforcement
- [ ] Update fuzz tests to demonstrate the difference
