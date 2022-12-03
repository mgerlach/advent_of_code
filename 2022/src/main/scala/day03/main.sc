import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day03/input.txt"))
val rucksacks = bufferedSource.getLines().toVector

bufferedSource.close

// Lowercase item types a through z have priorities 1 through 26.
// Uppercase item types A through Z have priorities 27 through 52.

def prio(item: Char) =
  if (item >= 'a') item.toInt - 96 else item.toInt - 38

// part 1

rucksacks
  .map(_.toVector)
  .map(contents =>
    contents.slice(0, contents.length / 2).toSet
      & contents.slice(contents.length / 2, contents.length).toSet)
  .map(_.head) // only one elem anyway
  .map(prio)
  .sum

// part 2

def sumGroupIntersections(group: Vector[String], rest: Vector[String]): Int =
// expect only one in the intersection of 3
  prio(group.map(_.toSet).reduce(_ & _).head)
    + (if (rest.nonEmpty) sumGroupIntersections(rest.take(3), rest.takeRight(rest.length - 3)) else 0)

sumGroupIntersections(rucksacks.take(3), rucksacks.takeRight(rucksacks.length - 3))

