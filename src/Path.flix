mod Path {

    use Path.{Path, Bot};

    pub enum Path[a] with ToString {
        case Path(List[a])
        case Bot // Infinitely long path
    }

    instance Eq[Path[a]] {
        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) == List.length(ys)
            case (Bot, _)             => false
            case (_, Bot)             => false
        }
    }

    instance Order[Path[a]] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) <=> List.length(ys)
            case (Bot, _)             => Comparison.GreaterThan
            case (_, Bot)             => Comparison.GreaterThan
        }
    }

    instance LowerBound[Path[a]] {
        // The longest list
        pub def minValue(): Path[a] = Bot
    }

    instance PartialOrder[Path[a]] {
        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Path(xs), Path(ys)) => List.length(xs) >= List.length(ys)
            case (Bot, _)             => false
            case (_, Bot)             => false
        }
    }

    instance JoinLattice[Path[a]] {
        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Path(xs), Path(ys)) => if (List.length(xs) <= List.length(ys)) x else y
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
        }
    }

    instance MeetLattice[Path[a]] {
        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Path(xs), Path(ys)) => if (List.length(xs) > List.length(ys)) x else y
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
        }
    }

    ///
    /// Returns a `Path` from `x` to `y`.
    ///
    pub def init(y: a, x: a): Path[a] =
        Path(y :: x :: Nil)

    ///
    /// Extends the `Path` `p` with `z`.
    ///
    pub def cons(z: a, p: Path[a]): Path[a] = match p {
        case Path(xs) => Path(z :: xs)
        case Bot      => Bot
    }

    ///
    /// Returns `Some(p)` if `p` is a non-empty `Path`.
    ///
    pub def toOption(p: Path[a]): Option[Path[a]] = match p {
        case Path(xs) if List.isEmpty(xs) => None
        case Path(xs)                     => Some(Path(xs))
        case Bot                          => None
    }

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
        case Path(xs) => List.indexOf(x, xs)
        case Bot      => None
    }

}
