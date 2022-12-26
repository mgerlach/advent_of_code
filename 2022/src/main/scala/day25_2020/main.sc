import scala.collection.immutable.LazyList.iterate
import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day25_2020/input.txt"))
val publicKeys = bufferedSource.getLines().map(_.toInt).toIndexedSeq

bufferedSource.close

// Set the value to itself multiplied by the subject number.
// Set the value to the remainder after dividing the value by 20201227.

def getSequence(subjectNumber: Long): LazyList[Long] =
  iterate(1L)(i => (i * subjectNumber) % 20201227L)

def getLoopSize(subjectNumber: Long, pk: Long) =
  var i = 0
  var value = 1L
  while (value != pk)
    i += 1
    value = (value * subjectNumber) % 20201227L
  i

// fast
// var loopSizes = publicKeys.map(getLoopSize(7L, _))
// slow
var loopSizes = publicKeys.map(pk => getSequence(7L).takeWhile(_ != pk).length)

// slow
getSequence(publicKeys(0)).take(loopSizes(1) + 1).last
getSequence(publicKeys(1)).take(loopSizes(0) + 1).last // same result

// for fast we would need an iteration fn similar to getLoopSize