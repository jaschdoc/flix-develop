namespace RedBlackTree1 {
    use RedBlackTree.RedBlackTree.{Leaf, DoubleBlackLeaf, Node};
    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def mapWithKey(f: (k, v1) -> v2 & ef, tree: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match tree {
        case Node(color, left, k, v, right) => Node(color, mapWithKey(f, left), k, f(k, v), mapWithKey(f, right))
        case _                              => Leaf
    }

    pub def map(f: v1 -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        reifyEff(f) {
            case Pure(g) =>
                if (RedBlackTree.blackHeight(t) >= 10)
                    parMap(g, t)
                else
                    seqMap(g, t)
            case _       => seqMap(f, t)
        }

    pub def parMap(f: v1 -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] =
        let cpus = Environment.getVirtualProcessors();
        parMapSplit(cpus - 1, f, t) as & Pure

    pub def parMapSplit(n: Int32, f: v1 -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & Impure = {
        if (n <= 1)
            seqMap(f, t)
        else
            match t {
                case Leaf                   => Leaf
                case DoubleBlackLeaf        => DoubleBlackLeaf
                case Node(c, a, k, v, b)    =>
                    let chanL = chan RedBlackTree[k, v2] 1;
                    let chanR = chan RedBlackTree[k, v2] 1;
                    spawn (chanL <- parMapSplit((n - 2) / 2, f, a));
                    spawn (chanR <- parMapSplit((n - 2) / 2, f, b));
                    let v1 = f(v);
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    Node(c, a1, k, v1, b1)
            }
    }

    pub def seqMap(f: v1 -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match t {
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = seqMap(f, a);
            let v1 = f(v);
            let b1 = seqMap(f, b);
            Node(c, a1, k, v1, b1)
    }

}
