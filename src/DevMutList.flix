namespace DevMutList {
    def minSize(): Int32 = 8
    
    pub def range(b: Int32, e: Int32): MutList[Int32] & Impure =
        if (e - b < minSize())
            MutList(ref rangeHelperFill(b, e), ref minSize())

        else
            MutList(ref Array.range(b, e), ref (e - b))

    def rangeHelperFill(b: Int32, e: Int32): Array[Int32] & Impure = {
        fill([default; 8], 0, b, e)
    }

    def fill(a: Array[Int32], idx: Int32, c: Int32, e: Int32): Array[Int32] & Impure = {
            if (e - c == 0)
                a
            else
                fill(Array.put(a, idx, c), idx + 1, c + 1, e)
        }
}