mod Path {

    use Path.{Path, Bot};

    pub enum Path[a] with ToString {
        case Path(List[a])
        case Bot // Infinitely long path
    }

    instance Eq[Path[a]] {
        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, Bot)           => true
            case (Path(xs), Path(ys)) => List.length(xs) == List.length(ys)
            case _                    => false
        }
    }

    instance Order[Path[a]] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Bot, Bot)           => Comparison.EqualTo
            case (Bot, _)             => Comparison.LessThan
            case (_, Bot)             => Comparison.GreaterThan
            case (Path(xs), Path(ys)) => List.length(xs) <=> List.length(ys)
        }
    }

    instance LowerBound[Path[a]] {
        // The longest list
        pub def minValue(): Path[a] = Bot
    }

    instance PartialOrder[Path[a]] {
        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, Bot)           => true
            case (Path(xs), Path(ys)) => List.length(xs) >= List.length(ys)
            case _                    => false
        }
    }

    instance JoinLattice[Path[a]] {
        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, p)             => p
            case (p, Bot)             => p
            case (Path(xs), Path(ys)) => if (List.length(xs) <= List.length(ys)) x else y
        }
    }

    instance MeetLattice[Path[a]] {
        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
            case (Path(xs), Path(ys)) => if (List.length(xs) > List.length(ys)) x else y
        }
    }

    instance Foldable[Path] {

        pub def foldLeft(f: b -> (a -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldLeft(f, s, p)
        }

        pub def foldRight(f: a -> (b -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldRight(f, s, p)
        }

        pub def foldRightWithCont(f: a -> ((Unit -> b \ ef) -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldRightWithCont(f, s, p)
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
        case Bot      => Bot
        case Path(xs) => Path(z :: xs)
    }

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
        case Bot      => None
        case Path(xs) => List.indexOf(x, xs)
    }

}
