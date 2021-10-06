/* turn into test cases

    pub def ex1(): Int32 =
        let l1 = range(1, 10);              // l1 must be eager
        let l2 = filter(x -> x > 5, l1);    // l2 must be eager
        sum(l2)

    pub def ex2(): Int32 & Impure =
        let l1 = range(1, 10);                          // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);    // l2 must be eager
        sum(l2)

    pub def ex3(): Option[Int32] =
        let l1 = range(1, 10);                          // l1 must be lazy (i.e. a free variable and not true).
        let l2 = filter(x -> x > 5, l1);                // l2 must be lazy (i.e. a free variable and not true).
        head(l2)

    pub def ex4(): Option[Int32] & Impure =
        let l1 = range(1, 10);                            // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);      // l2 must be eager
        head(l2)


*/