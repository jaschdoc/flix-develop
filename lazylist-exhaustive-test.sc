import scala.annotation.tailrec

def testExhaustive[A](prefix: String, l: List[A], suffix: String): String = {

  @tailrec
  def te(l: List[A], acc: Set[(String, Int)]): Set[String] = l match {
    case Nil => acc.map(st => st._1 + s"ENil" + ")".repeat(st._2)) //add some @test annotation and stuff
    case x :: xs => te(xs,
      acc.map(st => (st._1 + s"ECons($x, ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LCons($x, lazy ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LList(lazy ECons($x, ", st._2 + 2))
        ++ acc.map(st => (st._1 + s"LList(lazy LCons($x, lazy ", st._2 + 2))
    )
  }

  val tests = te(l, Set(("", 1)))

  tests.foldLeft((0, Set[String]()))((acc, test) => {
    val (index, set) = acc
    (index + 1, set + ("@test\ndef test" + index + "\n" + prefix + test + suffix))
  })._2.mkString("\n\n")
}

println(
  testExhaustive(
    "LazyList.toList(",
    List(
      1
    ),
    ") == 1 :: Nil"
  )
)
