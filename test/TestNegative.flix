mod Test.Dec.Assoc.Type.Negative {

/*
trait C[a] {
    type S : Type
    type T : Type -> Type
    pub def f(x: a): C.T[a][C.S[a]]
}

instance C[Int32] {
    type S = Int32
    type T = Option
    pub def f(x: Int32): Option[Int64] = Some(x)
}

trait A[a] {
    type A : Type -> Type -> Type
    type B : Type -> Type
    type C : Type
    pub def f(x: a): A.A[a][A.A[a][A.B[a][A.A[a][A.B[a][A.C[a]]][A.B[a][A.C[a]]]]][A.B[a][A.C[a]]]][A.A[a][A.B[a][A.C[a]]][A.C[a]]]
    pub def g(x: a): A.A[a][A.B[a][A.C[a]]][A.C[a]] = A.f(x)
}


trait B[a] {
    type Q : Type -> Type
    type R : Type
    pub def g(x: a): B.Q[a][B.R[a]]
}

trait C[a] with B[a] {
    type S : Type
    type T : Type -> Type
    pub def f(x: a): C.T[a][B.Q[a][C.T[a][C.S[a]]]]
}
instance C[Int32] {
    type S = Int32
    type T = Option
    pub def f(x: Int32): Option[Int64] = x
}

trait Foo[t] {
    type K: Type
    type E: Type
    pub def f(x: t): Foo.K[t][Foo.E[t]]
}
*/
}
