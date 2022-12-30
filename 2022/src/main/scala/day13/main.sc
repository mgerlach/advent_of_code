import scala.collection.immutable.LazyList.iterate
import scala.io.Source

trait N extends Ordered[N]:
  def i: Int

  def list: List[N]

case class L(list: List[N]) extends N:
  override def toString: String = s"[${list.mkString(",")}]"

  override def i: Int = ??? // exception if called

  override def compare(other: N): Int =
    iterate((None.asInstanceOf[Option[Int]], this.list, other.list))((_, thisList, otherList) =>
      (thisList, otherList) match
        case (Nil, Nil) => (Some(0), thisList, otherList)
        case (Nil, _) => (Some(-1), thisList, otherList)
        case (_, Nil) => (Some(1), thisList, otherList)
        case (thisHead :: thisTail, otherHead :: otherTail) =>
          (Option(thisHead compare otherHead).filter(_ != 0), thisTail, otherTail)
    )
      .find((res, _, _) => res.nonEmpty)
      .flatMap((res, _, _) => res)
      .get

case class I(i: Int) extends N:
  override def toString: String = i.toString

  override def list = List(this)

  override def compare(other: N): Int =
    if (other.isInstanceOf[L])
      L(list).compare(other) // requires class L to be defined first
    else
      Integer.compare(i, other.i)

// returns parsed I(nteger) node and remaining string
def parseI(str: String): (I, String) =
  val intStr = str.takeWhile(_.isDigit)
  (I(intStr.toInt), str.takeRight(str.length - intStr.length))

// returns parsed L(ist) node and remaining string,
// '[' must have already been removed from str!
def parseL(str: String): (L, String) =
  iterate((List[N](), str, false))((res, rest, _) =>
    rest.head match
      case c if c.isDigit => parseI(rest) match
        case (i, r) => (res :+ i, r, false)
      case '[' => parseL(rest.tail) match
        case (l, r) => (res :+ l, r, false)
      case ',' => (res, rest.tail, false)
      case ']' => (res, rest.tail, true)
  )
    .find((_, _, done) => done)
    .map((res, rest, _) => (L(res), rest))
    .get

val bufferedSource = Source.fromURL(getClass.getResource("/day13/input.txt"))
val packetPairs = bufferedSource
  .mkString
  .split("\n\n")
  .map(_.split("\n")
    .map(packetStr => parseL(packetStr.tail) match // skip initial '['
      case (packet, rest) =>
        if (rest.nonEmpty)
          throw IllegalStateException(s"parse error for '$packetStr', packet $packet, rest '$rest''")
        packet)
    .toIndexedSeq)
  .toIndexedSeq

bufferedSource.close

// part 1

packetPairs
  .zipWithIndex
  .filter((pair, _) => pair(0) < pair(1))
  .map((_, index) => index + 1)
  .sum

// part 2

val divider1 = L(List(L(List(I(2)))))
val divider2 = L(List(L(List(I(6)))))

val sorted = (packetPairs.flatten ++ Seq(divider1, divider2)).sorted

(sorted.indexOf(divider1) + 1) * (sorted.indexOf(divider2) + 1)