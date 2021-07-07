namespace DevMutList {
    /////////////////////////////////////////////////////////////////////
    // 1. Change minimum size to be 8                                  //
    // 2. DONE Change toImm to use directly tail recursive function    //
    /////////////////////////////////////////////////////////////////////

    def minSize(): Int32 = 8

    pub def new(): MutList[a] & Impure = MutList(ref [default; minSize()], ref 0)
    
    pub def range(b: Int32, e: Int32): MutList[Int32] & Impure =
        if (e - b < minSize())
            MutList(ref rangeHelperFill(b, e), ref (e - b))

        else
            MutList(ref Array.range(b, e), ref (e - b))

    def rangeHelperFill(b: Int32, e: Int32): Array[Int32] & Impure = {
        fill([default; minSize()], 0, b, e)
    }

    def fill(a: Array[Int32], idx: Int32, c: Int32, e: Int32): Array[Int32] & Impure = {
            if (e - c == 0)
                a
            else
                fill(Array.put(a, idx, c), idx + 1, c + 1, e)
    }

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