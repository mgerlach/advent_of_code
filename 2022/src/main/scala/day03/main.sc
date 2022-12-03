import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day03/input.txt"))
val rucksacks = bufferedSource.getLines().toVector

bufferedSource.close

// Lowercase item types a through z have priorities 1 through 26.
// Uppercase item types A through Z have priorities 27 through 52.

def prio(item: Char) = item - (if (item >= 'a') 'a' - 1 else 'A' - 27)

// part 1

rucksacks
  .map(r => r.splitAt(r.length / 2).toList
    .map(_.toSet)
    .reduce(_ & _)
    .head) // should be only 1
  .map(prio)
  .sum

// part 2

def sumGroupIntersections(group: Vector[String], rest: Vector[String]): Int =
// expect only one in the intersection of 3
  prio(group.map(_.toSet).reduce(_ & _).head)
    + (if (rest.nonEmpty) sumGroupIntersections(rest.take(3), rest.takeRight(rest.length - 3)) else 0)

sumGroupIntersections(rucksacks.take(3), rucksacks.takeRight(rucksacks.length - 3))

