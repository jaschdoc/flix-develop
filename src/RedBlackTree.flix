namespace RedBlackTree1 {
    use RedBlackTree.RedBlackTree.{Leaf, DoubleBlackLeaf, Node};
    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def mapWithKeyOld(f: (k, v1) -> v2 & ef, tree: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match tree {
        case Node(color, left, k, v, right) => Node(color, mapWithKeyOld(f, left), k, f(k, v), mapWithKeyOld(f, right))
        case _                              => Leaf
    }

    // Note: The use of a multiplier. Using 1 thread per logic core is too little.
    def multiplier(): Int32 = 4

    pub def mapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        let cpus = multiplier() * Environment.getVirtualProcessors();
        let f1 = (r) -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                let g1 = (k, v) -> g({k = k, v = v});
                if (RedBlackTree.blackHeight(t) >= 4)
                    parMap(cpus - 1, g1, t) as & Pure
                else
                    seqMap(g1, t)
            case _  => seqMap(f, t)
        }

    pub def parMap(n: Int32, f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & Impure = {
        if (n <= 1)
            parMapSeq(f, t)
        else
            match t {
                case Leaf                   => Leaf
                case DoubleBlackLeaf        => DoubleBlackLeaf
                case Node(c, a, k, v, b)    =>
                    let chanL = chan RedBlackTree[k, v2] 1;
                    let chanR = chan RedBlackTree[k, v2] 1;
                    spawn (chanL <- parMap((n - 2) / 2, f, a));
                    spawn (chanR <- parMap((n - 2) / 2, f, b));
                    let v1 = f(k, v);
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    Node(c, a1, k, v1, b1)
            }
    }

    // Note that while this is identical to seqMap it must still be its own function
    // because the parallel path must not be reuse the seq. path.
    pub def parMapSeq(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match t {
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = parMapSeq(f, a);
            let v1 = f(k, v);
            let b1 = parMapSeq(f, b);
            Node(c, a1, k, v1, b1)
    }

    pub def seqMap(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match t {
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = seqMap(f, a);
            let v1 = f(k, v);
            let b1 = seqMap(f, b);
            Node(c, a1, k, v1, b1)
    }

    pub def map(f: v1 -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        mapWithKey((_, v) -> f(v), t)
}
