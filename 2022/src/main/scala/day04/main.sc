import scala.io.Source

case class Interval(start: Int, end: Int):
  def overlapsLeft(other: Interval) =
    this.start <= other.start && this.end >= other.start

  def overlapsRight(other: Interval) =
    this.end >= other.end && this.start <= other.end

  def contains(other: Interval) =
    (this overlapsLeft other) && (this overlapsRight other)

  def containedIn(other: Interval) = other contains this // not needed

class AssignmentPair(i1: Interval, i2: Interval):
  def intervalsOverlapTotally = (i1 contains i2) || (i2 contains i1)

  def intervalsOverlapPartially =
    (i1 overlapsLeft i2) || (i1 overlapsRight i2) ||
      (i2 overlapsLeft i1) || (i2 overlapsRight i1)

val bufferedSource = Source.fromURL(getClass.getResource("/day04/input.txt"))
val assignmentPairs = bufferedSource.getLines().map(_.split(Array(',', '-')).map(_.toInt))
  .map(i => AssignmentPair(Interval(i(0), i(1)), Interval(i(2), i(3))))
  .toVector

bufferedSource.close

// part 1
assignmentPairs.count(_.intervalsOverlapTotally)

// part 2
assignmentPairs.count(_.intervalsOverlapPartially)

