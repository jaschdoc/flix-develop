namespace RedBlackTree1 {
    use RedBlackTree.RedBlackTree.{Leaf, DoubleBlackLeaf, Node};
    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color.{Red, Black, DoubleBlack};

    pub def mapWithKeyOld(f: (k, v1) -> v2 & ef, tree: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match tree {
        case Node(color, left, k, v, right) => Node(color, mapWithKeyOld(f, left), k, f(k, v), mapWithKeyOld(f, right))
        case _                              => Leaf
    }

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    def threads(): Int32 =
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Environment.getVirtualProcessors()

    ///
    /// Determines whether to use parallel evaluation.
    ///
    /// By default we only enable parallel evaluation if the tree has a certain size.
    ///
    def useParallelEvaluation(t: RedBlackTree[k, v]): Bool =
        let minSize = 2 ** RedBlackTree.blackHeight(t);
        minSize >= 1024

    ///
    /// Returns a RedBlackTree with mappings `k -> f(k, v)` for every `k -> v` in `t`.
    ///
    /// Purity polymorphic: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        let f1 = (r) -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                let g1 = (k, v) -> g({k = k, v = v});
                if (useParallelEvaluation(t))
                    parMapWithKey(threads() - 1, g1, t) as & Pure
                else
                    seqMapWithKey(g1, t)
            case _  => seqMapWithKey(f, t)
        }

    ///
    /// Maps `f` over the tree `t` in parallel.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    pub def parMapWithKey(n: Int32, f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & Impure = {
        if (n <= 1)
            parMapWithKeySeq(f, t)
        else
            match t {
                case Leaf                   => Leaf
                case DoubleBlackLeaf        => DoubleBlackLeaf
                case Node(c, a, k, v, b)    =>
                    let chanL = chan RedBlackTree[k, v2] 1;
                    let chanR = chan RedBlackTree[k, v2] 1;
                    spawn (chanL <- parMapWithKey((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parMapWithKey((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = f(k, v);                                  // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    Node(c, a1, k, v1, b1)
            }
    }

    ///
    /// Maps `f` over the tree `t` sequentially from left to right.
    ///
    /// Note that this is equivalent to `seqMapWithKey` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def parMapWithKeySeq(f: (k, v1) -> v2, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] = match t {
        // Note that while this is identical to `seqMapWithKey` it must still be its own function
        // because the parallel path must not be reuse the seq. path.
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = parMapWithKeySeq(f, a);
            let v1 = f(k, v);
            let b1 = parMapWithKeySeq(f, b);
            Node(c, a1, k, v1, b1)
    }

    ///
    /// Sequentially maps `f` over the tree `t`.
    ///
    def seqMapWithKey(f: (k, v1) -> v2 & ef, t: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef = match t {
        case Leaf                   => Leaf
        case DoubleBlackLeaf        => DoubleBlackLeaf
        case Node(c, a, k, v, b)    =>
            let a1 = seqMapWithKey(f, a);
            let v1 = f(k, v);
            let b1 = seqMapWithKey(f, b);
            Node(c, a1, k, v1, b1)
    }
}
