import scala.annotation.tailrec

class ExhaustiveTest(val tests: Set[String]) {
  override def toString: String = tests.mkString("\n\n")
}

def testExhaustive[A](l: List[A]): ExhaustiveTest = {

  @tailrec
  def te(l: List[A], acc: Set[(String, Int)]): Set[String] = l match {
    case Nil => acc.map(st => st._1 + s"ENil" + ")".repeat(st._2)) //add some @test annotation and stuff
    case ::(head, tail) => te(tail,
      acc.map(st => (st._1 + s"ECons($head, ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LCons(x, lazy ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LList(lazy ECons(x, ", st._2 + 2))
        ++ acc.map(st => (st._1 + s"LList(lazy LCons(x, lazy ", st._2 + 2))
    )
  }

  new ExhaustiveTest(te(l, Set()))
}

testExhaustive(
  List(
    1,
    2,
    3,
  )
)
