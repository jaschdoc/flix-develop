mod EffectExamples.NonDeterminism {

    eff Failure {
        pub def fail(): Void
    }

    eff Decision {
        pub def decide(): Bool
    }

    pub def decide(): Bool \ Decision =
        do Decision.decide()

    pub def chooseTrue(f: Unit -> a \ Decision): a =
        try {
            f()
        } with Decision {
            def decide(k) = k(true)
        }

    pub def chooseMax(f: Unit -> Int32 \ Decision): Int32 =
        try {
            f()
        } with Decision {
            def decide(k) = Int32.max(k(true), k(false))
        }

    pub def chooseAll(f: Unit -> a \ Decision + Failure): List[a] =
        try {
            f() :: Nil
        } with Failure {
            def fail(_) = Nil
        } with Decision {
            def decide(k) = k(true) ++ k(false)
        }


    pub def chooseInt(m: Int32, n: Int32): Int32 \ Failure + Decision =
        if (m > n)
            do Failure.fail()
        else if (decide())
            m
        else chooseInt(m + 1, n)

    @Test
    pub def testChooseTrue01(): Bool =
        let x = chooseTrue(() -> if (decide()) 10 else 20);
        let y = chooseTrue(() -> if (decide()) 0 else 5);
        Assert.eq(10, x - y)

    @Test
    pub def testChooseMax01(): Bool =
        let x = chooseMax(() -> if (decide()) 10 else 20);
        let y = chooseMax(() -> if (decide()) 0 else 5);
        Assert.eq(15, x - y)

    @Test
    pub def testChooseAll01(): Bool =
        let l =
            foreach (
                x <- chooseAll(() -> checked_ecast(if (decide()) 10 else 20));
                y <- chooseAll(() -> checked_ecast(if (decide()) 0 else 5))
            ) yield x - y;
        Assert.eq(10 :: 5 :: 20 :: 15 :: Nil, l)

}
