sealed trait Status

case object Todo extends Status {
  override def toString: String = ":heavy_multiplication_x:"
}

case object Done extends Status {
  override def toString: String = ":heavy_check_mark:"
}

case object Warning extends Status {
  override def toString: String = ":warning:"
}

case object SNA extends Status {
  override def toString: String = ":question:"
}

sealed trait Difficulty

case object Easy extends Difficulty {
  override def toString: String = ":green_circle:"
}

case object Hard extends Difficulty {
  override def toString: String = ":red_circle:"
}

case object DNA extends Difficulty {
  override def toString: String = ":question:"
}

type Name = String
type Comment = String

case class FunctionProgress(name: Name, iterStatus: Status, listStatus: Status, difficulty: Difficulty, comment: Comment) {
  val _name = name.trim
  require(_name.nonEmpty)
}

implicit def fpOrdering: Ordering[FunctionProgress] = Ordering.by(f => f.name)

case object FunctionProgress {
  def from(name: Name, iterStatus: Status = SNA, listStatus: Status = SNA, difficulty: Difficulty = DNA, comment: Comment = ""): FunctionProgress = {
    FunctionProgress(name.trim, iterStatus, listStatus, difficulty, comment)
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
          |    - $DNA Unknown, will be updated (${fps.count(fp => fp.difficulty == DNA)} / ${fps.length})
          |
          || Function | Iterator ($iterDone $Done / ${fps.length} and $iterWarning $Warning) | LazyList ($listDone $Done / ${fps.length} and $listWarning $Warning) | Difficulty | Comment |
          || :------: | :------:                                                             | :------:                                                             | :--------: | :-----: |
          |""".stripMargin.replaceAll(" {2}", "")

    fps.foldLeft(title)((acc, fp) => {
      acc.appendedAll(s"| `${fp.name}` | ${fp.iterStatus} | ${fp.listStatus} | ${fp.difficulty} | ${fp.comment} |\n")
    }).trim
  }
}


println(
  Set(
    FunctionProgress.from("isEmpty", Done, Done, Easy),
    FunctionProgress.from("append", Todo, Todo, Hard),
    FunctionProgress.from("filterMap", Todo, Todo, DNA),
    FunctionProgress.from("findMap", Todo, Todo, DNA),
    FunctionProgress.from("reduceLeft", Todo, Todo, Easy),
    FunctionProgress.from("reduceRight", Todo, Todo, Easy),
    FunctionProgress.from("intersperse", Todo, Todo, Hard),
    FunctionProgress.from("minimum", Todo, Todo, Easy),
    FunctionProgress.from("minimumBy", Todo, Todo, Easy),
    FunctionProgress.from("maximum", Todo, Todo, Easy),
    FunctionProgress.from("maximumBy", Todo, Todo, Easy),
    FunctionProgress.from("mapWithIndex", Todo, Todo, Easy),
    FunctionProgress.from("flatMap", Todo, Todo, Easy),
    FunctionProgress.from("intercalate", Todo, Todo, Hard),
    FunctionProgress.from("flatten", Todo, Todo, DNA),
    FunctionProgress.from("partition", Todo, Todo, Hard),
    FunctionProgress.from("span", Todo, Todo, Hard),
    FunctionProgress.from("findMap", Todo, Todo, DNA),
    FunctionProgress.from("count", Done, Todo, Easy),
    FunctionProgress.from("drop", Done, Todo, Easy),
    FunctionProgress.from("take", Done, Done, Easy),
    FunctionProgress.from("map", Done, Todo, Easy),
    FunctionProgress.from("filter", Done, Todo, Easy),
    FunctionProgress.from("findLeft", Done, Todo, Easy),
    FunctionProgress.from("findRight", Warning, Todo, Easy, "Needs optimization"),
    FunctionProgress.from("head", Done, Done, Easy),
    FunctionProgress.from("range", Done, Done, Easy),
    FunctionProgress.from("repeat", Done, Todo, Easy),
    FunctionProgress.from("memberOf", Done, Todo, Easy),
    FunctionProgress.from("toArray", Done, Todo, Easy),
    FunctionProgress.from("toMap", Done, Todo, Easy),
    FunctionProgress.from("toSet", Done, Todo, Easy),
    FunctionProgress.from("replace", Done, Todo, Easy),
    FunctionProgress.from("exists", Done, Todo, Easy),
    FunctionProgress.from("foreach", Done, Todo, Easy),
    FunctionProgress.from("forall", Done, Todo, Easy),
    FunctionProgress.from("dropWhile", Warning, Todo, Hard, "`dropWhileL` not implemented yet"),
    FunctionProgress.from("takeWhile", Warning, Todo, Hard, "`takeWhileL` not implemented yet"),
    FunctionProgress.from("zip", Done, Todo, Easy),
    FunctionProgress.from("zipWith", Warning, Todo, Easy, "`zipWithE` requires optimization"),
    FunctionProgress.from("foldLeft", Done, Todo, Easy),
    FunctionProgress.from("foldRight", Done, Todo, Easy),
    FunctionProgress.from("toList", Done, Done, Easy),
    FunctionProgress.from("from", SNA, Todo, Easy, "Is this even relevant for `Iterator`?"),
    FunctionProgress.from("empty", SNA, Done, Easy, "Not to be confused with `isEmpty`. Is this even relevant for `Iterator`?"),
  ).toList.sorted.toMarkdown
)
