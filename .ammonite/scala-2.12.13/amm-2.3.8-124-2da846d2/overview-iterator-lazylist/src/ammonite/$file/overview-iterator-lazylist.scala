
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


object `overview-iterator-lazylist`{
/*<script>*/sealed trait Choice

case object Yes extends Choice {
  override def toString: String = ":heavy_check_mark:"
}

case object No extends Choice {
  override def toString: String = ":heavy_multiplication_x:"
}

case object Maybe extends Choice {
  override def toString: String = ":question:"
}

sealed trait Status

sealed trait Difficulty

case object Todo extends Status {
  override def toString: String = ":heavy_multiplication_x:"
}

case object Done extends Status {
  override def toString: String = ":heavy_check_mark:"
}

case object Warning extends Status {
  override def toString: String = ":warning:"
}


case object Easy extends Difficulty {
  override def toString: String = ":green_circle:"
}

case object Hard extends Difficulty {
  override def toString: String = ":red_circle:"
}

case object NA extends Status with Difficulty {
  override def toString: String = ":question:"
}


type Name = String
type Comment = String

case class FunctionProgress(name: Name, iterStatus: Status,
                            listStatus: Status,
                            difficulty: Difficulty,
                            polymorphic: Choice,
                            comment: Comment) {
  require(name.trim.nonEmpty)

  override def toString: String =
    s"| $name | $iterStatus | $listStatus | $difficulty | $polymorphic | $comment |"
}

implicit def fpOrdering: Ordering[FunctionProgress] = Ordering.by(f => f.name)

case object FunctionProgress {
  def from(name: Name,
           iterStatus: Status = NA,
           listStatus: Status = NA,
           difficulty: Difficulty = NA,
           polymorphic: Choice = No,
           comment: Comment = ""): FunctionProgress = {
    FunctionProgress(
      name.trim,
      iterStatus,
      listStatus,
      difficulty,
      polymorphic,
      comment)
  }
}

implicit class Stringifier(fps: List[FunctionProgress]) {
  def toMarkdown: String = {
    val iterDone = fps.count(fp => fp.iterStatus == Done)
    val iterWarning = fps.count(fp => fp.iterStatus == Warning)
    val listDone = fps.count(fp => fp.listStatus == Done)
    val listWarning = fps.count(fp => fp.listStatus == Warning)
    val title =
      s"""|# Status Overview
          |
          |Difficulty explanation
          |    - $Easy Expected to be easy to implement (${fps.count(fp => fp.difficulty == Easy)} / ${fps.length})
          |    - $Hard Requires thought / consideration (${fps.count(fp => fp.difficulty == Hard)} / ${fps.length})
          |    - $NA Unknown, will be updated (${fps.count(fp => fp.difficulty == NA)} / ${fps.length})
          |
          || Function | Iterator ($iterDone $Done / ${fps.length} and $iterWarning $Warning) | LazyList ($listDone $Done / ${fps.length} and $listWarning $Warning) | Difficulty | Polymorphic eager/lazy| Comment |
          || :------: | :------:                                                             | :------:                                                             | :--------: | :---------:           | :-----: |
          |""".stripMargin.replaceAll(" {2}", "")

    fps.map(_.toString).mkString(title, "\n", "").trim
  }
}


/*<amm>*/val res_18 = /*</amm>*/println(
  Set(
    FunctionProgress.from("isEmpty", Done, Done, Easy),
    FunctionProgress.from("append", Todo, Todo, Hard),
    FunctionProgress.from("filterMap", Todo, Todo, NA, polymorphic = Yes),
    FunctionProgress.from("findMap", Todo, Todo, NA, polymorphic = Yes),
    FunctionProgress.from("reduceLeft", Todo, Todo, Easy),
    FunctionProgress.from("reduceRight", Todo, Todo, Easy),
    FunctionProgress.from("intersperse", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("minimum", Todo, Todo, Easy),
    FunctionProgress.from("minimumBy", Todo, Todo, Easy),
    FunctionProgress.from("maximum", Todo, Todo, Easy),
    FunctionProgress.from("maximumBy", Todo, Todo, Easy),
    FunctionProgress.from("mapWithIndex", Todo, Todo, Easy, polymorphic = Yes),
    FunctionProgress.from("flatMap", Todo, Todo, Easy, polymorphic = Yes),
    FunctionProgress.from("intercalate", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("flatten", Todo, Todo, NA, polymorphic = Yes),
    FunctionProgress.from("partition", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("span", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("findMap", Todo, Todo, NA),
    FunctionProgress.from("count", Done, Todo, Easy),
    FunctionProgress.from("drop", Done, Todo, Easy, polymorphic = Yes),
    FunctionProgress.from("take", Done, Done, Easy, polymorphic = Yes),
    FunctionProgress.from("map", Done, Done, Easy, polymorphic = Yes),
    FunctionProgress.from("filter", Done, Todo, Easy, polymorphic = Yes),
    FunctionProgress.from("findLeft", Done, Todo, Easy),
    FunctionProgress.from("findRight", Warning, Todo, Easy, comment = "Needs optimization"),
    FunctionProgress.from("head", Done, Done, Easy),
    FunctionProgress.from("range", Done, Done, Easy),
    FunctionProgress.from("repeat", Done, Todo, Easy),
    FunctionProgress.from("memberOf", Done, Todo, Easy),
    FunctionProgress.from("toArray", Done, Todo, Easy),
    FunctionProgress.from("toMap", Done, Todo, Easy),
    FunctionProgress.from("toSet", Done, Todo, Easy),
    FunctionProgress.from("replace", Done, Todo, Easy, polymorphic = Maybe),
    FunctionProgress.from("exists", Done, Todo, Easy),
    FunctionProgress.from("foreach", Done, Todo, Easy),
    FunctionProgress.from("forall", Done, Todo, Easy),
    FunctionProgress.from("dropWhile", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("takeWhile", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("zip", Done, Todo, Easy, polymorphic = Yes),
    FunctionProgress.from("zipWith", Warning, Todo, Easy, polymorphic = Yes, comment = "`zipWithE` requires optimization"),
    FunctionProgress.from("foldLeft", Done, Todo, Easy),
    FunctionProgress.from("foldRight", Done, Todo, Easy),
    FunctionProgress.from("toList", Done, Done, Easy),
    FunctionProgress.from("from", NA, Todo, Easy, comment = "Is this even relevant for `Iterator`?"),
    FunctionProgress.from("empty", NA, Done, Easy, comment = "Not to be confused with `isEmpty`. Is this even relevant for `Iterator`?"),
    FunctionProgress.from("toIter", NA, Todo, Easy),
    FunctionProgress.from("toLazyList", Todo, NA, Easy, polymorphic = Maybe)
  ).toList.sorted.toMarkdown
)
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "overview$minusiterator$minuslazylist"
  /*</generated>*/
}
