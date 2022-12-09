import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day09/input.txt"))
val motionInput = bufferedSource.getLines().map(_.split(" ").toList).toVector

bufferedSource.close

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

  def -(other: Vec): Vec = Vec(this.x - other.x, this.y - other.y)

  def dist(other: Vec): Int = math.abs(this.x - other.x) + math.abs(this.y - other.y)

  def sgn: Vec = Vec(math.signum(this.x), math.signum(this.y))

val directionVectors = Map(
  "R" -> Vec(1, 0),
  "U" -> Vec(0, -1),
  "L" -> Vec(-1, 0),
  "D" -> Vec(0, 1)
)

val headMotions = motionInput.flatMap(m => m match
  case direction :: steps :: _ => (0 until steps.toInt).map(_ => directionVectors(direction))
  case _ => ???
)

val head = Vec(0, 0)

// part 1

val tail = head

def moveTail(head: Vec, tail: Vec): Vec =
  head dist tail match
    case 0 => tail
    case 1 => tail
    case 2 if tail.x != head.x && tail.y != head.y => tail
    case _ => tail + (head - tail).sgn

val headPosAndTailPath = headMotions.foldLeft((head, List(tail)))(
  (hAndTPath, hm) => hAndTPath match
    case (h, tPath) =>
      val newHead = h + hm
      (newHead, tPath :+ moveTail(newHead, tPath.last)))

Set(headPosAndTailPath._2: _*).size

// part 1 redone + part 2

val ropeLength = 10 // for part 1, use 2

val rope = (0 until ropeLength).map(_ => head).toList

def moveRope(m: Vec, rope: List[Vec]): List[Vec] =
  val newHead = rope.head + m
  newHead :: rope.tail.foldLeft((newHead, Nil.asInstanceOf[List[Vec]]))(
    (currHeadAndNewTail, knot) => currHeadAndNewTail match
      case (currHead, newTail) =>
        val newKnot = moveTail(currHead, knot)
        (newKnot, newTail :+ newKnot))._2

val ropeAndTailPath = headMotions.foldLeft((rope, List(rope.last)))(
  (rAndTPath, hm) => rAndTPath match
    case (r, tPath) =>
      val newRope = moveRope(hm, r)
      (newRope, tPath :+ newRope.last))

Set(ropeAndTailPath._2: _*).size