namespace ParallelSet {
    
    pub def count(f: a -> Bool & ef, s: Set[a]): Int32 & ef =
        if (reify ef)
            // parallelize
            let Set(rbTree) = s;
            let blackHeight = RedBlackTree.blackHeight(rbTree);
            let threshold = 2000;
            if (blackHeight > threshold)
                
                
        else
            Set.foldLeft((acc, x) -> if (f(x)) 1 + acc else acc, 0, xs) as & ef

}