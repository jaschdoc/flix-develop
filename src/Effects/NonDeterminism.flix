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

}
