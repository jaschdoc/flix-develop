mod EffectExamples.NonDeterminism {

    eff Failure {
        pub def fail(): Void
    }

    eff Decision {
        pub def decide(): Bool
    }

    pub def fail(): Unit \ Failure =
        match do Failure.fail() { case _ => unreachable!() }

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

}
