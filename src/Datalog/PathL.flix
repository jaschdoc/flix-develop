mod PathL {

    // Path with arbitrary lengths

    use Path.{Path, Bot};

    pub enum Path[a] with ToString {
        case Path(List[a], Int32)
        case Bot // Infinitely long path
    }

    instance Eq[Path[a]] {
        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, Bot)                 => true
            case (Path(_, l1), Path(_, l2)) => l1 == l2
            case _                          => false
        }
    }

    instance Order[Path[a]] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Bot, Bot)                 => Comparison.EqualTo
            case (Bot, _)                   => Comparison.LessThan
            case (_, Bot)                   => Comparison.GreaterThan
            case (Path(_, l1), Path(_, l2)) => l1 <=> l2
        }
    }

    instance LowerBound[Path[a]] {
        // The longest list
        pub def minValue(): Path[a] = Bot
    }

    instance PartialOrder[Path[a]] {
        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, _)                   => true
            case (Path(_, l1), Path(_, l2)) => l1 >= l2
            case _                          => false
        }
    }

    instance JoinLattice[Path[a]] {
        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, p)                   => p
            case (p, Bot)                   => p
            case (Path(_, l1), Path(_, l2)) => if (l1 <= l2) x else y
        }
    }

    instance MeetLattice[Path[a]] {
        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, _)                   => Bot
            case (_, Bot)                   => Bot
            case (Path(_, l1), Path(_, l2)) => if (l1 > l2) x else y
        }
    }

    instance Foldable[Path] {
        pub def foldLeft(f: b -> (a -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldLeft(f, s, p)
        }

        pub def foldRight(f: a -> (b -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldRight(f, s, p)
        }

        pub def foldRightWithCont(f: a -> ((Unit -> b \ ef) -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldRightWithCont(f, s, p)
        }
    }

    ///
    /// Returns a `Path` from `x` to `y` of length `l`.
    ///
    pub def init(y: a,  l: Int32, x: a): Path[a] =
        Path(y :: x :: Nil, l)

    ///
    /// Extends the `Path` `p` with `z` of length `l`.
    ///
    pub def cons(z: a, l: Int32, p: Path[a]): Path[a] = match (p) {
        case Bot          => Bot
        case Path(xs, l1) => Path(z :: xs, l1 + l)
    }

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
        case Bot         => None
        case Path(xs, _) => List.indexOf(x, xs)
    }

}
