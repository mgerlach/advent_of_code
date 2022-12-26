import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day09/input.txt"))
val motionInput = bufferedSource.getLines().map(_.split(" ").toList).toSeq

bufferedSource.close

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

  def -(other: Vec): Vec = Vec(this.x - other.x, this.y - other.y)

  def dist(other: Vec): Int = (this.x - other.x).abs + (this.y - other.y).abs

  def sgn: Vec = Vec(this.x.sign, this.y.sign)

val directionVectors = Map(
  "R" -> Vec(1, 0),
  "U" -> Vec(0, -1),
  "L" -> Vec(-1, 0),
  "D" -> Vec(0, 1)
)

val headMotions = motionInput.flatMap(m => m match
  case direction :: steps :: _ => (0 until steps.toInt).map(_ => directionVectors(direction)))

def moveTail(head: Vec, tail: Vec): Vec =
  head dist tail match
    case 0 => tail
    case 1 => tail
    case 2 if tail.x != head.x && tail.y != head.y => tail
    case _ => tail + (head - tail).sgn

// part 1

val headPosAndTailPath = headMotions.foldLeft((Vec(0, 0), Seq(Vec(0, 0))))((_, _) match
  case ((h, tPath), hm) =>
    val newHead = h + hm
    (newHead, tPath :+ moveTail(newHead, tPath.last)))

Set(headPosAndTailPath._2: _*).size

// part 1 redone + part 2

def part2(ropeLength: Int): Int =

  def moveRope(m: Vec, rope: Seq[Vec]): IndexedSeq[Vec] =
    val newHead = rope.head + m
    newHead +: rope.tail.foldLeft((newHead, IndexedSeq[Vec]()))((_, _) match
      case ((currHead, newTail), knot) =>
        val newKnot = moveTail(currHead, knot)
        (newKnot, newTail :+ newKnot))._2

  val rope = (0 until ropeLength).map(_ => Vec(0, 0))

  val ropeAndTailPath = headMotions.foldLeft((rope, IndexedSeq(rope.last)))((_, _) match
    case ((r, tPath), hm) =>
      val newRope = moveRope(hm, r)
      (newRope, tPath :+ newRope.last))

  Set(ropeAndTailPath._2: _*).size

// part 1
part2(2)

// part 2
part2(10)