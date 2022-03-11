namespace RegionTest {

    opaque type RegTest[t: Type, r: Region] = ScopedRef[t, r]

    def empty(r: Region[r]): RegTest[a, r] & reg(r) = region r {
        RegTest(ref $DEFAULT$)
    }

}
