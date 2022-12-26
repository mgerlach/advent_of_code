import scala.collection.immutable.LazyList.iterate
import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day25/input.txt"))
val snafuLines = bufferedSource.getLines().toSeq

bufferedSource.close

def toNumber(snafu: String): Long =
  snafu
    .map(Map('=' -> -2L, '-' -> -1L, '0' -> 0L, '1' -> 1L, '2' -> 2L))
    .reduce((d1, d0) => d1 * 5 + d0)

def toSnafu(number: Long): String =
  iterate(
    // First digit by just % 5
    // Normal base 5: additional digit at 5, 25, 125, ...
    // Here at 3 (1=), 13 (1==), 63 (1===), ...
    // i. e. at 5 - 2, at 25 - 12 (base 5: 22), at 125 - 62 (base 5: 222), etc.
    ((number + 2) / 5, Seq((number % 5).toInt)))(
    (rest, snafuDigits) => ((rest + 2) / 5, (rest % 5).toInt +: snafuDigits)
  )
    .find((rest, _) => rest == 0) match
    case Some((_, snafuDigits)) =>
      snafuDigits.map(d => IndexedSeq("0", "1", "2", "=", "-")(d)).mkString
    case _ => ???

// part 1

toSnafu(snafuLines.map(toNumber).sum)

