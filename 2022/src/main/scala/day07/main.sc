import scala.:+
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day07/input.txt"))
val instructions = bufferedSource.getLines().map(_.split(' ').toList).toList

bufferedSource.close

trait Node:
  def size: Int

  def name: String

  def add(node: Node): Unit

  def children: ArrayBuffer[Node]

  def isNonEmptyDir: Boolean = children.nonEmpty

case class File(name: String, size: Int) extends Node:

  override def add(node: Node): Unit = ??? // illegal

  override val children = ArrayBuffer()

case class Dir(name: String, children: ArrayBuffer[Node] = ArrayBuffer()) extends Node:
  override def size: Int = children.map(_.size).sum

  override def add(node: Node): Unit = children.addOne(node)

val root = Dir("/") // assume first command is "$ cd /"

def buildNodeGetRest(instructions: List[List[String]], current: Node): List[List[String]] =
  var remainingInstructions = instructions
  while (remainingInstructions.nonEmpty)
    remainingInstructions match
      case line :: rest =>
        remainingInstructions = rest
        line match
          case "$" :: "cd" :: "/" :: _ => ???
          case "$" :: "cd" :: ".." :: _ => return rest
          case "$" :: "cd" :: dirName :: _ =>
            // requires ls before cd
            remainingInstructions = buildNodeGetRest(remainingInstructions.tail, current.children.find(_.name == dirName).get)
          case "$" :: "ls" :: _ => // ls is a noop
          case "dir" :: name :: _ =>
            current.add(Dir(name)) // assume no dir is listed twice
          case sizeStr :: name :: _ =>
            current.add(File(name, sizeStr.toInt)) // dito

  remainingInstructions

buildNodeGetRest(instructions.tail, root) // returns empty list - no more lines to parse

root

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
