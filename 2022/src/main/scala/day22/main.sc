import scala.collection.immutable.LazyList.iterate
import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day22/input.txt"))
val mapAndInstructions = bufferedSource.mkString.split("\n\n")
bufferedSource.close

val instructions = mapAndInstructions(1)
val map = mapAndInstructions(0).split("\n").toIndexedSeq

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

val R = Vec(1, 0)
val D = Vec(0, 1)
val L = Vec(-1, 0)
val U = Vec(0, -1)

val turns: Map[Char, Map[Vec, Vec]] = Map(
  'R' -> Map(R -> D, D -> L, L -> U, U -> R),
  'L' -> Map(R -> U, U -> L, L -> D, D -> R))

val facingScore: Map[Vec, Int] = Map(R -> 0, D -> 1, L -> 2, U -> 3)

def takeNumber(instructions: String): (Int, String) =
  if (instructions.matches("\\d+"))
    (instructions.toInt, "")
  else
    iterate((instructions.head.toString, instructions.tail))((numStr, rest) => (numStr + rest.head, rest.tail))
      .takeWhile((numStr, rest) => numStr.matches("\\d+") && rest.nonEmpty)
      .last match
      case (numStr, rest) => (numStr.toInt, rest)

def takeRotation(instructions: String): (Char, String) = (instructions.head, instructions.tail)

def tile(pos: Vec) = map(pos.y)(pos.x)

// part 1

case class Rng(from: Int, until: Int):
  def includes(i: Int) = from <= i && i < until

// find valid ranges for each row and column
val xRange = map.map(r => Rng(r.indexWhere(_ != ' '), r.lastIndexWhere(_ != ' ') + 1))
val width = xRange.maxBy(_.until).until
val height = map.length
val yRange = (0 until width).map(x =>
  Rng((0 until height).indexWhere(y => xRange(y).includes(x)),
    (0 until height).lastIndexWhere(y => xRange(y).includes(x)) + 1))

def move(pos: Vec, facing: Vec, steps: Int): Vec =
  iterate(pos)(pos =>
    (pos + facing, facing) match
      case (Vec(x, y), R) => if (xRange(y).includes(x)) Vec(x, y) else Vec(xRange(y).from, y)
      case (Vec(x, y), L) => if (xRange(y).includes(x)) Vec(x, y) else Vec(xRange(y).until - 1, y)
      case (Vec(x, y), D) => if (yRange(x).includes(y)) Vec(x, y) else Vec(x, yRange(x).from)
      case (Vec(x, y), U) => if (yRange(x).includes(y)) Vec(x, y) else Vec(x, yRange(x).until - 1)
  )
    .take(steps + 1)
    .takeWhile(tile(_) == '.')
    .last

def part1 =
  val (steps, restInstr) = takeNumber(instructions)
  val pos = move(Vec(xRange(0).from, 0), R, steps)

  val (finalPos, finalFacing, _) =
    iterate((pos, R, restInstr))((pos, facing, instr) =>
      val (turn, tmpRestInstr) = takeRotation(instr)
      val newFacing = turns(turn)(facing)
      val (steps, restInstr) = takeNumber(tmpRestInstr)
      (move(pos, newFacing, steps), newFacing, restInstr)
    )
      .find((_, _, restInstr) => restInstr.isEmpty)
      .get

  println(s"$finalPos $finalFacing")
  1000 * (finalPos.y + 1) + 4 * (finalPos.x + 1) + facingScore(finalFacing)

part1

// part 2

val flip = Map(R -> L, L -> R, U -> D, D -> U)

// wrap definitions: (pos, facing) -> (pos, facing) on connected cube face

// for test data (4x4)
val testWrap: Map[(Vec, Vec), (Vec, Vec)] = (0 until 4)
  .flatMap(i => Seq(
    ((Vec(8 + i, 0), U), (Vec(4 - i, 4), D)),
    ((Vec(8, i), L), (Vec(4 + i, 4), D)),
    ((Vec(11, i), R), (Vec(15, 11 - i), L)),
    ((Vec(11, 4 + i), R), (Vec(15 - i, 8), D)),
    ((Vec(4 + i, 7), D), (Vec(8, 11 - i), R)),
    ((Vec(i, 7), D), (Vec(11 - i, 11), U)),
    ((Vec(0, 4 + i), L), (Vec(12 + i, 11), U))
  ))
  .flatMap(_ match // this part adds the opposite wrap for each wrap defined above
    case ((p1, f1), (p2, f2)) => Seq(
      ((p1, f1), (p2, f2)),
      ((p2, flip(f2)), (p1, flip(f1)))
    ))
  .toMap

// for real data (50x50)
val realWrap: Map[(Vec, Vec), (Vec, Vec)] = (0 until 50)
  .flatMap(i => Seq(
    ((Vec(50 + i, 0), U), (Vec(0, 199 - i), R)),
    ((Vec(100 + i, 0), U), (Vec(i, 199), U)),
    ((Vec(50, i), L), (Vec(0, 149 - i), R)),
    ((Vec(149, i), R), (Vec(99, 149 - i), L)),
    ((Vec(50, 50 + i), L), (Vec(i, 100), D)),
    ((Vec(99, 50 + i), R), (Vec(100 + i, 49), U)),
    ((Vec(49, 150 + i), R), (Vec(50 + i, 149), U))
  ))
  .flatMap(_ match /// this part adds the opposite wrap for each wrap defined above
    case ((p1, f1), (p2, f2)) => Seq(
      ((p1, f1), (p2, f2)),
      ((p2, flip(f2)), (p1, flip(f1)))
    ))
  .toMap

// return (new pos, new facing)
def move2(pos: Vec, facing: Vec, steps: Int, wrap: Map[(Vec, Vec), (Vec, Vec)]): (Vec, Vec) =
  iterate((pos, facing))((pos, facing) => wrap.getOrElse((pos, facing), (pos + facing, facing)))
    .take(steps + 1)
    .takeWhile((pos, _) => tile(pos) == '.')
    .last

def part2(wrap: Map[(Vec, Vec), (Vec, Vec)]) =
  val (steps, restInstr) = takeNumber(instructions)
  val (pos, facing) = move2(Vec(xRange(0).from, 0), R, steps, wrap)

  val ((finalPos, finalFacing), _) =
    iterate(((pos, facing), restInstr))(_ match
      case ((pos, facing), instr) =>
        val (turn, tmpRestInstr) = takeRotation(instr)
        val newFacing = turns(turn)(facing)
        val (steps, restInstr) = takeNumber(tmpRestInstr)
        (move2(pos, newFacing, steps, wrap), restInstr)
    )
      .find((_, restInstr) => restInstr.isEmpty)
      .get

  println(s"$finalPos $finalFacing")
  1000 * (finalPos.y + 1) + 4 * (finalPos.x + 1) + facingScore(finalFacing)

part2(realWrap)
