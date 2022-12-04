import scala.io.Source

case class Interval(start: Int, end: Int)

case class AssignmentPair(i1: Interval, i2: Interval)

val bufferedSource = Source.fromURL(getClass.getResource("/day04/input.txt"))
val assignmentPairs = bufferedSource.getLines().map(_.split(Array(',', '-')).map(_.toInt))
  .map(i => AssignmentPair(Interval(i(0), i(1)), Interval(i(2), i(3)))) // without this, see commented out code
  .toVector

bufferedSource.close

// part 1 - total overlap
//assignmentPairs.count(
// a => a(0) >= a(2) && a(1) <= a(3) ||
// a(0) <= a(2) && a(1) >= a(3)
// )
assignmentPairs.count(a =>
  a.i1.start >= a.i2.start && a.i1.end <= a.i2.end ||
    a.i1.start <= a.i2.start && a.i1.end >= a.i2.end
)

// part 2 - partial overlaps
//assignmentPairs.count(
// a => a(0) <= a(2) && a(1) >= a(2) ||
// a(1) >= a(3) && a(0) <= a(3) ||
// a(2) <= a(0) && a(3) >= a(0) ||
// a(3) >= a(1) && a(2) <= a(1)
// )
assignmentPairs.count(a =>
  a.i1.start <= a.i2.start && a.i1.end >= a.i2.start ||
    a.i1.end >= a.i2.end && a.i1.start <= a.i2.end ||
    a.i2.start <= a.i1.start && a.i2.end >= a.i1.start ||
    a.i2.end >= a.i1.end && a.i2.start <= a.i1.end
)
