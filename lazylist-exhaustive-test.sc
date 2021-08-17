import scala.annotation.tailrec

def testExhaustive[A](l: List[A]): String = {

  @tailrec
  def te(l: List[A], acc: Set[(String, Int)]): Set[String] = l match {
    case Nil => acc.map(st => st._1 + s"ENil" + ")".repeat(st._2)) //add some @test annotation and stuff
    case x :: xs => te(xs,
      acc.map(st => (st._1 + s"ECons($x, ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LCons(x, lazy ", st._2 + 1))
        ++ acc.map(st => (st._1 + s"LList(lazy ECons(x, ", st._2 + 2))
        ++ acc.map(st => (st._1 + s"LList(lazy LCons(x, lazy ", st._2 + 2))
    )
  }

  te(l, Set()).mkString("\n\n")
}

println(
  testExhaustive(
    List(
      1,
      2,
      3,
      4,
      5,
    )
  )
)
