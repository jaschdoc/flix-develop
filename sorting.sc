sealed trait Status
case object Todo extends Status {
  override def toString: String = ":heavy_multiplication_x:"
}
case object Done extends Status {
  override def toString: String = ":heavy_check_mark:"
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

case class FunctionProgress(name: Name, iterStatus: Status, lListStatus: Status, difficulty: Difficulty, comment: Comment) {
  val _name = name.trim
  require(_name.nonEmpty)
}

implicit def fpOrdering: Ordering[FunctionProgress] = Ordering.by(f => f.name)

case object FunctionProgress {
  def from(name: Name, iterStatus: Status = SNA, lListStatus: Status = SNA, difficulty: Difficulty = DNA, comment: Comment = ""): FunctionProgress = {
    FunctionProgress(name.trim, iterStatus, lListStatus, difficulty, comment)
  }
}

implicit class Stringifier(fps: List[FunctionProgress]) {
  def toMarkdown: String = {
    val title =
      """
        | # Status Overview
        |
        | Difficulty explanation
        |     - :green_circle: Expected to be easy to implement
        |     - :red_circle: Requires thought / consideration
        |     - :question: Unknown, will be updated
        |
        || Function | Iterator | LazyList | Difficulty | Comment |
        || :------: | :------: | :------: | :--------: | :-----: |
        |""".stripMargin

    fps.foldLeft(title)((acc, fp) => {
      acc.appendedAll(s"| ${fp.name} | ${fp.iterStatus} | ${fp.lListStatus} | ${fp.difficulty} | ${fp.comment} |\n")
    })
  }
}


println(
  Set(
    FunctionProgress.from("isEmpty", Done, Done),
    FunctionProgress.from("append", Todo, Todo),
    FunctionProgress.from("filterMap", Todo, Todo),
    FunctionProgress.from("findMap", Todo, Todo),
    FunctionProgress.from("reduceLeft", Todo, Todo),
    FunctionProgress.from("reduceRight", Todo, Todo),
    FunctionProgress.from("intersperse", Todo, Todo),
    FunctionProgress.from("minimum", Todo, Todo),
    FunctionProgress.from("minimumBy", Todo, Todo),
    FunctionProgress.from("maximum", Todo, Todo),
    FunctionProgress.from("maximumBy", Todo, Todo),
    FunctionProgress.from("mapWithIndex", Todo, Todo),
    FunctionProgress.from("flatMap", Todo, Todo),
    FunctionProgress.from("intercalate", Todo, Todo),
    FunctionProgress.from("flatten", Todo, Todo),
    FunctionProgress.from("partition", Todo, Todo),
    FunctionProgress.from("span", Todo, Todo),
    FunctionProgress.from("findMap", Todo, Todo),
    FunctionProgress.from("count", Todo, Todo),
    FunctionProgress.from("drop", Todo, Todo),
    FunctionProgress.from("take", Todo, Todo),
    FunctionProgress.from("map", Todo, Todo),
    FunctionProgress.from("filter", Todo, Todo),
    FunctionProgress.from("findLeft", Todo, Todo),
    FunctionProgress.from("findRight", Todo, Todo),
    FunctionProgress.from("head", Todo, Todo),
    FunctionProgress.from("range", Todo, Todo),
    FunctionProgress.from("repeat", Todo, Todo),
    FunctionProgress.from("memberOf", Todo, Todo),
    FunctionProgress.from("toArray", Todo, Todo),
    FunctionProgress.from("toMap", Todo, Todo),
    FunctionProgress.from("toSet", Todo, Todo),
    FunctionProgress.from("replace", Todo, Todo),
    FunctionProgress.from("exists", Todo, Todo),
    FunctionProgress.from("foreach", Todo, Todo),
    FunctionProgress.from("forall", Todo, Todo),
    FunctionProgress.from("dropWhile", Todo, Todo),
    FunctionProgress.from("takeWhile", Todo, Todo),
    FunctionProgress.from("zip", Todo, Todo),
    FunctionProgress.from("zipWith", Todo, Todo),
    FunctionProgress.from("foldLeft", Todo, Todo),
    FunctionProgress.from("foldRight", Todo, Todo),
    FunctionProgress.from("toList", Todo, Todo),
    FunctionProgress.from("from", Todo, Todo),
    FunctionProgress.from("new", Todo, Todo),
    FunctionProgress.from("empty", Todo, Todo),
  ).toList.sorted.toMarkdown
)
