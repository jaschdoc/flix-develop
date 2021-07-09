namespace DevMutList {
    /////////////////////////////////////////////////////////////////////
    // 1. Change minimum size to be 8                                  //
    // 2. DONE Change toImm to use directly tail recursive function    //
    /////////////////////////////////////////////////////////////////////

    def minSize(): Int32 = 8

    pub def new(): MutList[a] & Impure = MutList(ref [default; minSize()], ref 0)
    
    pub def range(b: Int32, e: Int32): MutList[Int32] & Impure =
        let min = minSize();
        if (e - b < min)
            let f = i -> if (b + i < e) b + i else default;
            MutList(ref Array.init(f, min), ref (e - b))
            
        else
            MutList(ref Array.range(b, e), ref (e - b))
            
    ///
    /// Returns `v` as an immutable list.
    ///
    pub def toImm(v: MutList[a]): List[a] & Impure =
        toImmHelper(v, Nil)

    ///
    /// Helper function for `toImm`
    ///
    def toImmHelper(v: MutList[a], acc: List[a]): List[a] & Impure =
        if (MutList.isEmpty(v))
            acc
        else
            let x = match MutList.pop!(v) {
                case Some(a) => a
                case _       => unreachable!()
            };
            toImmHelper(v, x :: acc)
}