///
/// From OCaml version 5.1 chapter 12
///
mod EffectExamples.Threads {

    eff Exchangable {
        pub def exchange(n: Int32): Int32
    }

    def comp1(): Int32 \ Exchangable =
        do Exchangable.exchange(0) + do Exchangable.exchange(1)

    def handleComp1(): Int32 =
        try {
            comp1()
        } with Exchangable {
            def exchange(n, k) = k(n + 1)
        }

    @Test
    pub def testComp1(): Bool =
        Assert.eq(3, handleComp1())

}
