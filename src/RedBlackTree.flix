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
        let f1 = r -> f(r.k, r.v);
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

    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    /// Purity polymorphic: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Int32 & ef =
        let f1 = r -> f(r.k, r.v);
        reifyEff(f1) {
            case Pure(g) =>
                let g1 = (k, v) -> g({k = k, v = v});
                if (useParallelEvaluation(t))
                    parCount(threads() - 1, g1, t) as & Pure
                else
                    seqCount(g1, t)
            case _  => seqCount(f, t)
        }

    ///
    /// Applies `f` over the tree `t` in parallel and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    /// The implementation spawns `n` threads each applying `f` sequentially
    /// from left to right on some subtree that is disjoint from the rest of
    /// the threads.
    ///
    pub def parCount(n: Int32, f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 & Impure = {
        if (n <= 1)
            parCountSeq(f, t)
        else
            match t {
                case Leaf                   => 0
                case DoubleBlackLeaf        => 0
                case Node(_, a, k, v, b)    =>
                    let chanL = chan Int32 1;
                    let chanR = chan Int32 1;
                    spawn (chanL <- parCount((n - 2) / 2, f, a)); // We divide the rest of the threads as follows:
                    spawn (chanR <- parCount((n - 2) / 2, f, b)); // We spawn two new threads leaving us with n - 2
                    let v1 = if (f(k, v)) 1 else 0;               // that we distribute over the two spanned threads.
                    let a1 = <- chanL;
                    let b1 = <- chanR;
                    a1 + v1 + b1
            }
    }

    ///
    /// Applies `f` over the tree `t` sequentially from left to right and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    /// Note that this is equivalent to `seqCount` but for performance reasons -- to avoid
    /// megamorphic calls -- we use a copy here.
    ///
    def parCountSeq(f: (k, v) -> Bool, t: RedBlackTree[k, v]): Int32 = match t {
        case Leaf                   => 0
        case DoubleBlackLeaf        => 0
        case Node(_, a, k, v, b)    =>
            let a1 = seqCount(f, a);
            let v1 = if (f(k, v)) 1 else 0;
            let b1 = seqCount(f, b);
            a1 + v1 + b1
    }

    ///
    /// Applies `f` over the tree `t` sequentially from left to right and returns the number of elements
    /// that satisfy the predicate `f`.
    ///
    pub def seqCount(f: (k, v) -> Bool & ef, t: RedBlackTree[k, v]): Int32 & ef = match t {
        case Leaf                   => 0
        case DoubleBlackLeaf        => 0
        case Node(_, a, k, v, b)    =>
            let a1 = seqCount(f, a);
            let v1 = if (f(k, v)) 1 else 0;
            let b1 = seqCount(f, b);
            a1 + v1 + b1
    }

    ///
    /// Optionally finds `k -> v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity polymorphic: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def minimumBy(cmp: (k, v, k, v) -> Comparison & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef =
        let f = r -> cmp(r.k1, r.v1, r.k2, r.v2);
        reifyEff(f) {
            case Pure(g) =>
                let g1 = (k1, v1, k2, v2) -> g({k1 = k1, v1 = v1, k2 = k2, v2 = v2});
                if (useParallelEvaluation(t))
                    parMinimumBy(threads() - 1, g1, t) as & Pure
                else
                    seqMinimumBy(g1, t)
            case _       => seqMinimumBy(cmp, t)
        }

    pub def parMinimumBy(n: Int32, cmp: (k, v, k, v) -> Comparison, t: RedBlackTree[k, v]): Option[(k, v)] & Impure =
        if (n <= 0)
            parSeqMinimumBy(cmp, t)
        else
            match t {
                case Leaf                => None
                case DoubleBlackLeaf     => None
                case Node(_, a, k, v, b) =>
                    let chanL = chan Option[(k, v)] 1;
                    let chanR = chan Option[(k, v)] 1;
                    spawn (chanL <- parMinimumBy((n - 2) / 2, cmp, a));
                    spawn (chanR <- parMinimumBy((n - 2) / 2, cmp, b));
                    def smallest(kl, vl, kr, vr) = if (cmp(kl, vl, kr, vr) == LessThan) (kl, vl) else (kr, vr);
                    let a1 = <- chanL;
                    let res = match a1 {
                        case None         => (k, v)
                        case Some(kl, vl) => smallest(kl, vl, k, v)
                    };
                    let b1 = <- chanR;
                    match b1 {
                        case None         => Some(res)
                        case Some(kr, vr) =>
                            let (ks, vs) = res;
                            Some(smallest(ks, vs, kr, vr))
                    }
            }

    // TODO: Naming scheme should follow the other similarly named function or they should be changed to match this
    pub def parSeqMinimumBy(cmp: (k, v, k, v) -> Comparison & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef = match t {
        case Leaf                   => None
        case DoubleBlackLeaf        => None
        case Node(_, a, k, v, b)    =>
            def smallest(kl, vl, kr, vr) = if (cmp(kl, vl, kr, vr) == LessThan) (kl, vl) else (kr, vr);
            let res = match seqMinimumBy(cmp, a) {
                case None         => (k, v)
                case Some(kl, vl) => smallest(kl, vl, k, v)
            };
            match seqMinimumBy(cmp, b) {
                case None         => Some(res)
                case Some(kr, vr) =>
                    let (ks, vs) = res;
                    Some(smallest(ks, vs, kr, vr))
            }
    }

    pub def seqMinimumBy(cmp: (k, v, k, v) -> Comparison & ef, t: RedBlackTree[k, v]): Option[(k, v)] & ef = match t {
        case Leaf                   => None
        case DoubleBlackLeaf        => None
        case Node(_, a, k, v, b)    =>
            def smallest(kl, vl, kr, vr) = if (cmp(kl, vl, kr, vr) == LessThan) (kl, vl) else (kr, vr);
            let res = match seqMinimumBy(cmp, a) {
                case None         => (k, v)
                case Some(kl, vl) => smallest(kl, vl, k, v)
            };
            match seqMinimumBy(cmp, b) {
                case None         => Some(res)
                case Some(kr, vr) =>
                    let (ks, vs) = res;
                    Some(smallest(ks, vs, kr, vr))
            }
    }
}
