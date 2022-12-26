import scala.collection.immutable.LazyList.iterate
import scala.io.Source

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

val bufferedSource = Source.fromURL(getClass.getResource("/day23/input.txt"))
val positions = bufferedSource.getLines()
  .zipWithIndex
  .flatMap((l, y) => l
    .zipWithIndex
    .filter((c, _) => c == '#')
    .map((_, x) => Vec(x, y)))
  .toSet

bufferedSource.close

val N = Vec(0, -1)
val NE = Vec(1, -1)
val E = Vec(1, 0)
val SE = Vec(1, 1)
val S = Vec(0, 1)
val SW = Vec(-1, 1)
val W = Vec(-1, 0)
val NW = Vec(-1, -1)

val allDirs = Seq(N, NE, E, SE, S, SW, W, NW)
val checks = IndexedSeq(
  (Seq(N, NE, NW), N),
  (Seq(S, SE, SW), S),
  (Seq(W, NW, SW), W),
  (Seq(E, NE, SE), E))

def round(positions: Set[Vec], counter: Int): Set[Vec] =

  def proposeForPos(pos: Vec): Option[Vec] =

    def count(pos: Vec, checks: Seq[Vec]): Int =
      checks.count(check => positions.contains(pos + check))

    if (count(pos, allDirs) == 0)
      None
    else
      checks.indices
        .map(i => (counter + i) % checks.length)
        .map(checks(_))
        .find((checkDirs, _) => count(pos, checkDirs) == 0)
        .map((_, proposedDir) => proposedDir)

  val propositions = positions.map(pos => (pos, proposeForPos(pos).map(pos + _).getOrElse(pos))).toMap

  val actualMoves: Map[Vec, Vec] = propositions
    .groupBy((_, targetPos) => targetPos)
    .map((_, proposedMoves) => proposedMoves)
    .filter(_.size == 1) // eliminate moves to same target
    .reduce(_ ++ _)

  positions.map(pos => actualMoves.getOrElse(pos, pos))

// part 1 check after 10 rounds

val part1Positions = iterate((positions, 0))((positions, counter) => (round(positions, counter), counter + 1))
  .take(11)
  .last match
  case (finalPositions, _) => finalPositions

val xs = part1Positions.map(_.x)
val ys = part1Positions.map(_.y)

(xs.max - xs.min + 1) * (ys.max - ys.min + 1) - part1Positions.size

// part 2 check how many rounds until no more change

iterate((Set[Vec](), positions, 0))((_, positions, counter) => (positions, round(positions, counter), counter + 1))
  .find((prevPosns, positions, _) => positions == prevPosns)
  .map((_, _, counter) => counter)
  .get