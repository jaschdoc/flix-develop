sealed trait Choice

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
    s"| `$name` | $iterStatus | $listStatus | $difficulty | $polymorphic | $comment |"

}

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

implicit def fpOrdering: Ordering[FunctionProgress] = Ordering.by(f => f.name)

implicit class Stringifier(fps: List[FunctionProgress]) {
  def toMarkdown: String = {
    val iterDone = fps.count(fp => fp.iterStatus == Done)
    val iterWarning = fps.count(fp => fp.iterStatus == Warning)
    val listDone = fps.count(fp => fp.listStatus == Done)
    val listWarning = fps.count(fp => fp.listStatus == Warning)
    val totalCount = fps.length
    val title =
      s"""|# Status Overview
          |
          |Difficulty explanation
          |    - $Easy Expected to be easy to implement (${fps.count(fp => fp.difficulty == Easy)} / $totalCount)
          |    - $Hard Requires thought / consideration (${fps.count(fp => fp.difficulty == Hard)} / $totalCount)
          |    - $NA Unknown, will be updated (${fps.count(fp => fp.difficulty == NA)} / $totalCount)
          |
          || Function | Iterator ($iterDone $Done / ${fps.filterNot(fp => fp.iterStatus == NA).length} and $iterWarning $Warning) | LazyList ($listDone $Done / ${fps.filterNot(fp => fp.listStatus == NA).length} and $listWarning $Warning) | Difficulty | Polymorphic eager/lazy (${fps.count(fp => fp.polymorphic == Yes)} / $totalCount) | Comment |
          || :------: | :------:                                                             | :------:                                                             | :--------: | :---------:           | :-----: |
          |""".stripMargin.replaceAll(" {2}", "")

    fps.map(_.toString).mkString(title, "\n", "").trim
  }
}


println(
  List(
    FunctionProgress.from("isEmpty", Done, Done, Easy),
    FunctionProgress.from("append", Todo, Done, Easy),
    FunctionProgress.from("filterMap", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("findMap", Todo, Todo, Easy),
    FunctionProgress.from("reduceLeft", Todo, Done, Easy),
    FunctionProgress.from("reduceRight", Todo, Done, Easy),
    FunctionProgress.from("intersperse", Todo, Todo, Hard),
    FunctionProgress.from("minimum", Todo, Done, Easy),
    FunctionProgress.from("minimumBy", Todo, Done, Easy),
    FunctionProgress.from("maximum", Todo, Done, Easy),
    FunctionProgress.from("maximumBy", Todo, Done, Easy),
    FunctionProgress.from("mapWithIndex", Todo, Warning, Easy, polymorphic = Yes, comment = "Compiler bug with curried function and effect ef. See https://github.com/flix/flix/pull/2206"),
    FunctionProgress.from("flatMap", Todo, Done, Easy, polymorphic = Yes),
    FunctionProgress.from("intercalate", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("flatten", Todo, Done, Easy),
    FunctionProgress.from("partition", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("span", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("count", Done, Done, Easy),
    FunctionProgress.from("drop", Done, Done, Easy),
    FunctionProgress.from("take", Done, Done, Easy),
    FunctionProgress.from("map", Done, Done, Easy, polymorphic = Yes),
    FunctionProgress.from("filter", Done, Done, Easy, polymorphic = Yes),
    FunctionProgress.from("findLeft", Done, Done, Easy),
    FunctionProgress.from("findRight", Warning, Done, Easy),
    FunctionProgress.from("head", Done, Done, Easy),
    FunctionProgress.from("range", Done, Done, Easy),
    FunctionProgress.from("repeat", Done, Done, Easy),
    FunctionProgress.from("memberOf", Done, Done, Easy),
    FunctionProgress.from("toArray", Done, Done, Easy),
    FunctionProgress.from("toMap", Done, Done, Easy),
    FunctionProgress.from("toSet", Done, Done, Easy),
    FunctionProgress.from("replace", Done, Done, Easy),
    FunctionProgress.from("exists", Done, Done, Easy),
    FunctionProgress.from("foreach", Done, Done, Easy),
    FunctionProgress.from("forall", Done, Done, Easy),
    FunctionProgress.from("dropWhile", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("takeWhile", Todo, Todo, Hard, polymorphic = Yes),
    FunctionProgress.from("zip", Done, Done, Easy),
    FunctionProgress.from("zipWith", Warning, Done, Easy, polymorphic = Yes, comment = "`zipWithE` requires optimization"),
    FunctionProgress.from("foldLeft", Done, Done, Easy),
    FunctionProgress.from("foldRight", Done, Done, Easy),
    FunctionProgress.from("toList", Done, Done, Easy),
    FunctionProgress.from("from", NA, Done, Easy, comment = "Is this even relevant for `Iterator`?"),
    FunctionProgress.from("empty", NA, Done, Easy),
    FunctionProgress.from("toIter", NA, Done, Easy),
    FunctionProgress.from("toLazyList", Todo, NA, Easy),
    FunctionProgress.from("last", Todo, Done, Easy),
    FunctionProgress.from("reverse", NA, Done, Easy),
    FunctionProgress.from("length", Todo, Done, Easy),
    FunctionProgress.from("tail", Todo, Done, Easy),
  ).distinctBy(_.name.trim.toLowerCase).sorted.toMarkdown
)
