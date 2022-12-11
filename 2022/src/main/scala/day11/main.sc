import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Monkey(items: mutable.Queue[Long],
                  op: Long => Long,
                  divisor: Int,
                  throwTo: Long => Int):
  def inspect: Long = op(items.removeHead())

def readMonkeys(source: Source): Seq[Monkey] =
  def intAtEnd(s: String): Int = s.split(' ').last.toInt

  source.mkString
    .split("\n\n")
    .map(monkeyDef =>
      val monkeyDefLines = monkeyDef.split('\n').map(_.split(':').last.trim)
      val items = mutable.Queue(monkeyDefLines(1).split(',').map(_.trim.toLong): _*)
      val op =
        monkeyDefLines(2).split('=')(1).trim.split(' ').toList match
          case "old" :: "*" :: "old" :: _ => (i: Long)
            => i * i
          case "old" :: "*" :: operand :: _ => (i: Long)
            => i * operand.toInt
          case "old" :: "+" :: operand :: _ => (i: Long)
            => i + operand.toInt
      val divisor = intAtEnd(monkeyDefLines(3))
      val ifDivisible = intAtEnd(monkeyDefLines(4))
      val ifNotDivisible = intAtEnd(monkeyDefLines(5))
      val throwTo = (i: Long) => if (i % divisor == 0) ifDivisible else ifNotDivisible
      Monkey(items, op, divisor, throwTo)
    )

def monkeyBusiness(monkeys: Seq[Monkey], rounds: Int, reduceWorryLevel: Long => Long) =

  val inspectionCounts = ArrayBuffer(monkeys.map(_ => 0L): _*)
  for _ <- 1 to rounds do
    for m <- monkeys.indices do
      val monkey = monkeys(m)
      while (monkey.items.nonEmpty)
        val item = reduceWorryLevel(monkey.inspect) // .inspect mutates Monkey, must only be called once per item
        monkeys(monkey.throwTo(item)).items.append(item)
        inspectionCounts(m) += 1

  //monkeys.foreach(println)
  //println(inspectionCounts)
  inspectionCounts.sorted.takeRight(2).product

def part1 =
  val source = Source.fromURL(getClass.getResource("/day11/input.txt"))
  val monkeys = readMonkeys(source)
  source.close()

  // Worry level reduction: divide any operation result by 3
  monkeyBusiness(monkeys, 20, _ / 3)

part1

def part2 =
  val source = Source.fromURL(getClass.getResource("/day11/input.txt"))
  val monkeys = readMonkeys(source)
  source.close()

  // Worry level reduction: replace any operation result by the remainder of dividing it
  // by the product of all divisors used by the monkeys for divisibility tests.
  // The divisors are all prime and all different, but maybe that is not a requirement for this to work.
  // For a few rounds (~10) with some non-prime, non-unique divisors, % (product of divisors) worked as well.
  monkeyBusiness(monkeys, 10000, _ % monkeys.map(_.divisor.toLong).product)

part2

