import scala.collection.immutable.LazyList.iterate
import scala.collection.mutable
import scala.io.Source

case class Num(n: Long, i: Int):
  override def toString: String = n.toString

val bufferedSource = Source.fromURL(getClass.getResource("/day20/input.txt"))
val origNumbers = bufferedSource.getLines().zipWithIndex.map((n, i) => Num(n.toInt, i)).toIndexedSeq

val len = origNumbers.length
val shortLen = len - 1

def mix(min: Long, origNumbers: IndexedSeq[Num], input: IndexedSeq[Num]): IndexedSeq[Num] =
  // minimum difference needed to add to all (negative) indexes to stay within 0..(shortLen-1) with same % shortLen
  val correction = (1 - min / shortLen) * shortLen
  origNumbers.foldLeft(input)((numbers, currNumber) =>
    // println(s"${numbers.length} $numbers")
    val currIndex = numbers.indexOf(currNumber)
    val shortList = numbers.filterNot(_ == currNumber)
    val targetIndex = ((currIndex + currNumber.n + correction) % shortLen).toInt
    // println(s"$currNumber($currIndex -> $targetIndex)")
    (shortList.slice(0, targetIndex) :+ currNumber) ++ shortList.slice(targetIndex, shortLen))

// part 1

val numbers = mix(origNumbers.minBy(_.n).n, origNumbers, origNumbers)

val zero = numbers.indexWhere(_.n == 0)

val z1000 = numbers((zero + 1000) % len).n
val z2000 = numbers((zero + 2000) % len).n
val z3000 = numbers((zero + 3000) % len).n

z1000 + z2000 + z3000

// part 2

val origNumbersEnc = origNumbers.map(num => Num(num.n * 811589153L, num.i))

val numbersEnc =
  iterate(origNumbersEnc)(input => mix(origNumbersEnc.minBy(_.n).n, origNumbersEnc, input))
    .take(11)
    .last

val zeroEnc = numbersEnc.indexWhere(_.n == 0)

val zEnc1000 = numbersEnc((zeroEnc + 1000) % len).n
val zEnc2000 = numbersEnc((zeroEnc + 2000) % len).n
val zEnc3000 = numbersEnc((zeroEnc + 3000) % len).n

zEnc1000 + zEnc2000 + zEnc3000
