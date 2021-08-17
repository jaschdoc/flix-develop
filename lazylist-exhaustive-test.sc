import scala.annotation.tailrec

sealed trait Purity

case object Pure extends Purity {
  override def toString: String = ""
}

case object Impure extends Purity {
  override def toString: String = " & Impure"
}

implicit class toFlixLazyLists[+A](val l: List[A]) {
  def toLazyLists(limit: Option[Int] = None): Set[String] = {
    @tailrec
    def te(l: List[A], current: Long, max: Long, acc: Set[(String, Int)]): Set[String] = l match {
      case Nil => acc.map(st => st._1 + s"ENil" + ")".repeat(st._2)) ++ acc.map(st => st._1 + s"LList(lazy ENil)" + ")".repeat(st._2)) //add some @test annotation and stuff
      case _ :: _ if current >= max => te(Nil, current, max, acc.dropRight((current - max).toInt))
      case x :: xs => te(xs, current + 4, max,
        acc.map(st => (st._1 + s"ECons($x, ", st._2 + 1))
          ++ acc.map(st => (st._1 + s"LCons($x, lazy ", st._2 + 1))
          ++ acc.map(st => (st._1 + s"LList(lazy ECons($x, ", st._2 + 2))
          ++ acc.map(st => (st._1 + s"LList(lazy LCons($x, lazy ", st._2 + 2))
      )
    }

    val max = limit match {
      case Some(value) => value
      case None => (Math.pow(4, l.length) * 2).round
    }
    te(l, current = 0, max = max, Set(("", 0)))
  }
}

def testExhaustive[A](name: String, prefix: String, l: List[A], suffix: String, startFrom: Int = 1, purity: Purity = Pure, limit: Option[Int] = None): String = {
  val tests = l.toLazyLists()

  tests.foldLeft((if (startFrom < 1) 1 else startFrom, Set[(Int, String)]()))((acc, test) => {
    val (index, set) = acc
    (index + 1, set + ((index, "@test\ndef " + name + (if (index < 10) "0" + index else index.toString) + "(): Bool" + purity + " = \n\t" + prefix + test + suffix)))
  })._2.toList.sortBy(t => t._1).map(_._2).mkString("\n\n")
}

println(
  testExhaustive(
    name = "foldLeft",
    "LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ",
    List(1, 2, 3),
    ") == 386",
    startFrom = 11,
    purity = Pure,
    limit = Some(4),
  )
)
