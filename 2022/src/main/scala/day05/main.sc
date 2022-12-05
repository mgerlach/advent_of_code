import java.util.regex.Pattern
import scala.collection.mutable
import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day05/input.txt"))
val lines = bufferedSource.getLines().toVector
bufferedSource.close

val (stacksWithIndexes, instructionLines) = lines.splitAt(lines.indexOf(""))
val reversedStackLines = stacksWithIndexes.reverse.tail

def parseStacks(reversedStackLines: Vector[String]) =
  val stackMap = mutable.Map[Int, mutable.Stack[String]]()
  reversedStackLines.foreach(line =>
    (" " + line).grouped(4).zipWithIndex.foreach(crateWithIndex => crateWithIndex match
      case (crate, index) if crate.trim.nonEmpty =>
        val stack: mutable.Stack[String] = stackMap.getOrElse(index, mutable.Stack())
        stack.push(crate.substring(2, 3))
        stackMap(index) = stack
      case _ =>
    )
  )
  (0 until stackMap.size).map(stackMap(_)).toVector

class Instruction(num: Int, from: Int, to: Int):
  def moveOneAtATime(stacks: Vector[mutable.Stack[String]]): Unit =
    for _ <- 1 to num do stacks(to).push(stacks(from).pop())

  def moveAllAtOnce(stacks: Vector[mutable.Stack[String]]): Unit =
    val temp = mutable.Stack[String]()
    for _ <- 1 to num do temp.push(stacks(from).pop())
    for _ <- 1 to num do stacks(to).push(temp.pop())


val movePattern = Pattern.compile("move (\\d+) from (\\d+) to (\\d+)")

val instructions = instructionLines.tail
  .map(movePattern.matcher(_))
  .filter(_.matches())
  .map(m => Instruction(m.group(1).toInt, m.group(2).toInt - 1, m.group(3).toInt - 1))


// part 1

val stacks = parseStacks(reversedStackLines)

instructions.foreach(_.moveOneAtATime(stacks))

stacks.foldLeft("")(_ + _.pop())

// part 2

val stacks2 = parseStacks(reversedStackLines)

instructions.foreach(_.moveAllAtOnce(stacks2))

stacks2.foldLeft("")(_ + _.pop())
