namespace RedBlackTree {
    use RedBlackTree.RedBlackTree.{Leaf, DoubleBlackLeaf, Node};
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def mapWithKey(f: (k, v) -> b & ef, tree: RedBlackTree[k, v]): RedBlackTree[k, b] & ef = match tree {
        case Node(color, left, k, v, right) => Node(color, mapWithKey(f, left), k, f(k, v), mapWithKey(f, right))
        case _                              => Leaf
    }

}
