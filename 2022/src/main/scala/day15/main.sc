import scala.collection.immutable.LazyList.iterate
import scala.io.Source

case class Vec(x: Int, y: Int):
  def dist(other: Vec): Int = (this.x - other.x).abs + (this.y - other.y).abs

// Sensor at x=20, y=1: closest beacon is at x=15, y=3
val r = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

val bufferedSource = Source.fromURL(getClass.getResource("/day15/input.txt"))
val sensorToClosestBeacon = bufferedSource.getLines()
  .map(_ match
    case r(sx, sy, bx, by) => (Vec(sx.toInt, sy.toInt), Vec(bx.toInt, by.toInt)))
  .toList

bufferedSource.close

val sensorToClosestBeaconDist = sensorToClosestBeacon.map((s, b) => (s, s dist b))

val Y = 2000000 // 10
def intersectWithY(s: Vec, d: Int, y: Int): Option[Range] =
  Option(d - (s.y - y).abs)
    .filter(_ >= 0)
    .map(dx => s.x - dx to s.x + dx)

def rangesOnY(y: Int) =
  sensorToClosestBeaconDist
    .map((s, d) => intersectWithY(s, d, y))
    .filter(_.nonEmpty)
    .map(_.get)

// part 1

val beaconsOnY = sensorToClosestBeacon.toSet.map((_, b) => b).count(_.y == Y)

def mergeRanges(ranges: List[Range]): List[Range] =
  ranges.sortBy(_.start) match
    case head :: tail => tail.foldLeft(List(head))((merged, range) =>
      if (merged.last.end >= range.start)
        merged.reverse.tail.reverse :+ (merged.last.start to math.max(merged.last.end, range.end))
      else
        merged :+ range
    )

val part1 = mergeRanges(rangesOnY(Y)).map(r => r.end - r.start + 1).sum - beaconsOnY

// part 2

val maxY = 4000000 // 20

val (y, ranges) = iterate(0)(_ + 1)
  .map(y => (y, mergeRanges(rangesOnY(y))))
  .find((y, ranges) => ranges.size == 2 || y > maxY /* make sure to stop if none found */)
  .get

val part2 = y + (ranges.head.end + 1) * 4000000L // The L is important, otherwise Scala just cuts off at 32 bits!
