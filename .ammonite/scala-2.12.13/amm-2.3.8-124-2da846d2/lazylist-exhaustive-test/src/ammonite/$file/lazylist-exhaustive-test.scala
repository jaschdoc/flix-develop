
package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}


object `lazylist-exhaustive-test`{
/*<script>*/import scala.annotation.tailrec

sealed trait Purity

case object Pure extends Purity {
  override def toString: String = ""
}

case object Impure extends Purity {
  override def toString: String = " & Impure"
}

implicit class toFlixLazyLists[+A](val l: List[A]) {

  def toLazyLists: Set[String] = {
    @tailrec
    def te(l: List[A], acc: Set[(String, Int)]): Set[String] = l match {
      case Nil => acc.map(st => st._1 + s"ENil" + ")".repeat(st._2)) ++ acc.map(st => st._1 + s"LList(lazy ENil)" + ")".repeat(st._2))
      case x :: xs => te(xs,
        acc.map(st => (st._1 + s"ECons($x, ", st._2 + 1))
          ++ acc.map(st => (st._1 + s"LCons($x, lazy ", st._2 + 1))
          ++ acc.map(st => (st._1 + s"LList(lazy ECons($x, ", st._2 + 2))
          ++ acc.map(st => (st._1 + s"LList(lazy LCons($x, lazy ", st._2 + 2))
      )
    }

    te(l, Set(("", 0)))
  }
}

def testExhaustive[A](name: String, prefix: String, l: List[A], suffix: String, startFrom: Int = 1, purity: Purity = Pure): String = {
  val tests = l.toLazyLists

  tests.foldLeft((if (startFrom < 1) 1 else startFrom, Set[(Int, String)]()))((acc, test) => {
    val (index, set) = acc
    (index + 1, set + ((index, "@test\ndef " + name + (if (index < 10) "0" + index else index.toString) + "(): Bool" + purity + " = \n\t" + prefix + test + suffix)))
  })._2.toList.sortBy(t => t._1).map(_._2).mkString("\n\n")
}

/*<amm>*/val res_6 = /*</amm>*/println(
  "namespace GeneratedTests {\n\n" +
  testExhaustive(
    name = "intersperse",
    "LazyList1.intersperse(-11, ",
    List(1, 2, 3, 4, 5),
    ") == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ECons(-11, ECons(5, ENil)))))))))",
    startFrom = 171,
    purity = Pure,
  )
  + "\n\n}"
)
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "lazylist$minusexhaustive$minustest"
  /*</generated>*/
}
