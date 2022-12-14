import scala.io.Source

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

  def -(other: Vec): Vec = Vec(this.x - other.x, this.y - other.y)

  def sgn: Vec = Vec(math.signum(this.x), math.signum(this.y))

val bufferedSource = Source.fromURL(getClass.getResource("/day14/input.txt"))
val paths = bufferedSource.getLines()
  .map(_.split("->")
    .map(_.split(",").map(_.trim))
    .map(xy => Vec(xy(0).toInt, xy(1).toInt))
    .toList)
  .toList

bufferedSource.close

def pathSegmentPoints(start: Vec, end: Vec): Set[Vec] =
  LazyList.iterate(start)(_ + (end - start).sgn).takeWhile(_ != end).toSet + end

val allPathPoints = paths.map(path => path.tail
  .foldLeft((Set[Vec](), path.head))((_, _) match
    case ((points, prev), curr) => (points ++ pathSegmentPoints(prev, curr), curr)))
  .map((points, _) => points)
  .reduce(_ ++ _)

val maxY = paths.flatten.maxBy(_.y).y

val Dummy = Vec(-1, -1) // guard
val Start = Vec(500, 0)

def sandFall(occupied: Set[Vec], yLimit: Int): Vec =

  def sandNext(sandCurrent: Vec): Vec =
    LazyList(Vec(0, 1), Vec(-1, 1), Vec(1, 1), Vec(0, 0))
      .map(sandCurrent + _)
      .find(!occupied.contains(_))
      .get // cannot be empty, as at least the current point (move (0, 0)) is not occupied!

  val (_, sandEnd) = LazyList
    .iterate((Dummy, Start))((_, curr) => (curr, sandNext(curr)))
    .find((prev, curr) => prev == curr || curr.y == maxY + yLimit)
    .get // cannot be empty because of condition

  sandEnd

// part 1 (start counter at -1 so we exclude the first unit of sand that would fall into the abyss)

val (part1, _, _) = LazyList
  .iterate((-1, allPathPoints, Dummy))((i, occupied, _) =>
    val sandEnd = sandFall(occupied, 0) // stop every unit at maxY (about to fall into abyss)
    (i + 1, occupied + sandEnd, sandEnd))
  .find((_, _, sandEnd) => sandEnd.y == maxY) // stop flow when the first unit reaches maxY (one above abyss)
  .get

// part 2 (start counter at 0 because we need to include the first unit of sand that remains at the start point)

val (part2, _, _) = LazyList
  .iterate((0, allPathPoints, Dummy))((i, occupied, _) =>
    val sandEnd = sandFall(occupied, 1) // stop every unit of sand on last row over floor (floor = maxY + 2)
    (i + 1, occupied + sandEnd, sandEnd))
  .find((_, _, sandEnd) => sandEnd == Start) // stop flow when the first unit remains at the start point (incl.)
  .get

