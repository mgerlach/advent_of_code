import scala.annotation.tailrec
import scala.io.Source

abstract class Node(val name: String):
  def eval: Long

  def contains(name: String): Boolean

case class Leaf(override val name: String, n: Long) extends Node(name):
  override def eval = n

  override def contains(name: String) = this.name == name

  override def toString: String = n.toString

abstract class BinaryOp(override val name: String, val left: Node, val right: Node) extends Node(name):
  override def contains(name: String) = this.left.contains(name) || this.right.contains(name)

case class Diff(override val name: String, override val left: Node, override val right: Node) extends BinaryOp(name, left, right):
  override def eval: Long = left.eval - right.eval

  override def toString: String = s"($left - $right)"

case class Add(override val name: String, override val left: Node, override val right: Node) extends BinaryOp(name, left, right):
  override def eval: Long = left.eval + right.eval

  override def toString: String = s"($left + $right)"

case class Div(override val name: String, override val left: Node, override val right: Node) extends BinaryOp(name, left, right):
  override def eval: Long = left.eval / right.eval

  override def toString: String = s"($left / $right)"

case class Mul(override val name: String, override val left: Node, override val right: Node) extends BinaryOp(name, left, right):
  override def eval: Long = left.eval * right.eval

  override def toString: String = s"($left * $right)"

val bufferedSource = Source.fromURL(getClass.getResource("/day21/test_input.txt"))
val defs = bufferedSource.getLines().map(_.split(": ")).map(l => (l(0), l(1))).toMap
bufferedSource.close

val rNum = "(-?\\d+)".r
val rOp = "(\\w+) ([-+*/]) (\\w+)".r

def parse(name: String): Node =
  defs(name) match
    case rNum(nStr) => Leaf(name, nStr.toInt)
    case rOp(left, "+", right) => Add(name, parse(left), parse(right))
    case rOp(left, "-", right) => Diff(name, parse(left), parse(right))
    case rOp(left, "*", right) => Mul(name, parse(left), parse(right))
    case rOp(left, "/", right) => Div(name, parse(left), parse(right))

val root = parse("root").asInstanceOf[BinaryOp] // type cast for part 2

// returns: sub-tree containing node with name, eval of sub-tree not containing node with name

// part 1

root.eval

// part 2

val targetName = "humn"

@tailrec
def solve(target: Node, other: Node): Node =
  if (target.name == targetName)
    other
  else
    target match
      case Add(name, left, right) if left.contains(targetName) =>
        solve(left, Diff(name, other, right))
      case Add(name, left, right) if right.contains(targetName) =>
        solve(right, Diff(name, other, left))
      case Diff(name, left, right) if left.contains(targetName) =>
        solve(left, Add(name, other, right))
      case Diff(name, left, right) if right.contains(targetName) =>
        solve(right, Diff(name, left, other))
      case Mul(name, left, right) if left.contains(targetName) =>
        solve(left, Div(name, other, right))
      case Mul(name, left, right) if right.contains(targetName) =>
        solve(right, Div(name, other, left))
      case Div(name, left, right) if left.contains(targetName) =>
        solve(left, Mul(name, other, right))
      case Div(name, left, right) if right.contains(targetName) =>
        solve(right, Div(name, left, other))
      case _ => ???

val (target, other) = if (root.left.contains("humn")) (root.left, root.right) else (root.right, root.left)

val humn = solve(target, other)

humn.eval
