sealed trait Status
case object Todo extends Status
case object Done extends Status

sealed trait Difficulty
case object Easy extends Difficulty
case object Hard extends Difficulty
case object NA extends Difficulty

type Name = String
type Comment = String

case class FunctionProgress(name: Name, iterStatus: Status, lListStatus: Status, difficulty: Difficulty, comment: Comment) {

}

implicit def fpOrdering: Ordering[FunctionProgress] = Ordering.by(f => f.name)

case object FunctionProgress {
  def from(name: Name, iterStatus: Status = Todo, lListStatus: Status = Todo, difficulty: Difficulty = Easy, comment: Comment = ""): FunctionProgress =
    FunctionProgress(name, iterStatus, lListStatus, difficulty, comment)
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
    FunctionProgress.from("???", Todo, Todo),
    "drop",
    "take",
    "map",
    "filter",
    "findLeft",
    "findRight",
    "head",
    "range",
    "repeat",
    "memberOf",
    "toArray",
    "toMap",
    "toSet",
    "replace",
    "exists",
    "foreach",
    "forall",
    "dropWhile",
    "takeWhile",
    "zip",
    "zipWith",
    "foldLeft",
    "foldRight",
    "toList",
    "from",
    "new",
    "empty",
  ).toList.sorted
)
