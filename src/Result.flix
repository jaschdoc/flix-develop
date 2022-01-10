namespace Result1 {

    ///
    /// Returns an iterator over `r` with 1 element or an empty iterator if `r` is `Err`.
    ///
    pub def toIterator(r: Result[t, e]): Iterator[t] & Impure = match r {
        case Err(_) => Iterator1.empty()
        case Ok(x)  => Iterator1.singleton(x)
    }

    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    /// Return `Err(e)`, if `f` throws a Java `RuntimeException`,
    /// where `e` is the error message.
    ///
    pub def try(f: Unit -> a & ef): Result[a, String] & ef =
        try {
            Ok(f())
        } catch {
            case e: ##java.lang.RuntimeException =>
                import java.lang.Throwable.getMessage();
                Err(getMessage(e)) as & Pure
        }
}


namespace Result {

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def sum(r: Result[Int32, e]): Int32 =
        sumWith(x -> x, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def sumWith(f: t -> Int32 & ef, r: Result[t, e]): Int32 & ef = match r {
        case Err(_) => 0
        case Ok(v)  => f(v)
    }

}
