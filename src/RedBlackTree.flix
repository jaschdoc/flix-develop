namespace RedBlackTree1 {
    use RedBlackTree.RedBlackTree.{Leaf, DoubleBlackLeaf, Node};
    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def mapWithKey(f: (k, v1) -> v2 & ef, tree: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match tree {
        case Node(color, left, k, v, right) => Node(color, mapWithKey(f, left), k, f(k, v), mapWithKey(f, right))
        case _                              => Leaf
    }

}
