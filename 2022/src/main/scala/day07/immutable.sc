import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day07/input.txt"))
val instructions = bufferedSource.getLines().map(_.split(' ').toList).toList

trait Node:
  def size: Int

  def name: String

  def children: Seq[Node]

  def isNonEmptyDir: Boolean = children.nonEmpty

case class File(name: String, size: Int) extends Node:
  override val children = Seq()

case class Dir(name: String, children: Seq[Node]) extends Node:
  override lazy val size: Int = children.map(_.size).sum

def buildNode(instructions: List[List[String]]): Node =
  def buildNodeAndRest(instructions: List[List[String]]): (Node, List[List[String]]) =
    instructions match
      case instruction :: rest => instruction match
        case "$" :: "cd" :: dirName :: _ =>
          // skip ls, collect listed nodes, also append any trailing "$ cd ..", until the next "$"
          // no action needed for cd .., this is handled by the recursion
          val childLines = rest.tail.takeWhile(l => l.head != "$" || l(1) == "cd" && l(2) == "..")
          val afterChildLines = rest.takeRight(rest.length - childLines.length - 1)
          val (children, afterChildrenParsing) =
            childLines.foldLeft((Seq[Node](), afterChildLines))((_, _) match
              case ((nodes, afterChildLines), childLine) =>
                childLine match
                  case "dir" :: _ =>
                    val (newNode, newAfterChildLines) = buildNodeAndRest(afterChildLines)
                    (nodes :+ newNode, newAfterChildLines)
                  case "$" :: _ => (nodes, afterChildLines) // skip "$ cd .."
                  case sizeStr :: name :: _ => (nodes :+ File(name, sizeStr.toInt), afterChildLines))
          (Dir(dirName, children), afterChildrenParsing)

  buildNodeAndRest(instructions)._1

val root = buildNode(instructions)

// part 1
val sizeLimit = 100000

def sumUpSmallNodes(node: Node): Int =
  node.children.filter(_.isNonEmptyDir).map(sumUpSmallNodes).sum +
    (if (node.size < sizeLimit) node.size else 0)

sumUpSmallNodes(root)

// part 2
val totalDiskSpace = 70000000
val neededSpace = 30000000
val unusedSpace = totalDiskSpace - root.size
val requiredDelete = neededSpace - unusedSpace

def findSmallestDir(node: Node, currentSmallest: Node): Node =
  if (node.isNonEmptyDir)
    val newSmallest = if (node.size > requiredDelete && node.size < currentSmallest.size) node else currentSmallest
    node.children.map(findSmallestDir(_, newSmallest)).minBy(_.size)
  else
    currentSmallest

findSmallestDir(root, root).size
