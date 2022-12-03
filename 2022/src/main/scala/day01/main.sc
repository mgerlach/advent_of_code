import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day01/input.txt"))

val sums = bufferedSource
  .mkString
  .split("\n\n")
  .map(_.split("\n").map(_.toInt).sum)

// part 1
sums.max

// part 2
sums.sorted.takeRight(3).sum

bufferedSource.close
