import scala.io.Source

trait N extends Ordered[N]:
  def i: Int

  def list: List[N]

case class L(list: List[N]) extends N:
  override def toString: String = s"[${list.mkString(",")}]"

  override def i: Int = ??? // exception if called

  override def compare(other: N): Int =
    var otherList = if (other.isInstanceOf[L]) other.list else List(other)
    var thisList = this.list
    var res = 0 // assume equality
    var done = false
    while (res == 0 && !done)
      (thisList, otherList) match
        case (Nil, Nil) =>
          done = true
        case (Nil, _) =>
          res = -1
        case (_, Nil) =>
          res = 1
        case (thisHead :: thisTail, otherHead :: otherTail) =>
          res = thisHead.compare(otherHead)
          thisList = thisTail
          otherList = otherTail
    res

case class I(i: Int) extends N:
  override def toString: String = i.toString

  override def list = ???

  override def compare(other: N): Int =
    if (other.isInstanceOf[L])
      L(List(this)).compare(other) // requires class L to be defined first
    else
      Integer.compare(i, other.i)

// returns parsed I(nteger) node and remaining string
def parseI(str: String): (I, String) =
  val intStr = str.takeWhile(_.isDigit)
  (I(intStr.toInt), str.takeRight(str.length - intStr.length))

// returns parsed L(ist) node and remaining string,
// '[' must have already been removed from str!
def parseL(str: String): (L, String) =
  var res = List[N]()
  var rest = str
  var done = false
  while (!done)
    rest.head match
      case c if c.isDigit => parseI(rest) match
        case (i, r) =>
          res = res :+ i
          rest = r
      case '[' => parseL(rest.tail) match
        case (l, r) =>
          res = res :+ l
          rest = r
      case ',' =>
        rest = rest.tail
      case ']' =>
        rest = rest.tail
        done = true

  (L(res), rest)

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