import scala.io.Source

// read from file
val bufferedSource = Source.fromURL(getClass.getResource("/day00/input.txt"))
val lines = bufferedSource.getLines().map(_.split(", *").toVector).toVector

println(lines)

bufferedSource.close

// new indentation syntax
case class State(n: Int, minValue: Int, maxValue: Int):

  def inc: State =
    if n == maxValue then
      this
    else
      this.copy(n = n + 1)

  def printAll(): Unit =
    println("Printing all")
    for
      i <- minValue to maxValue
      j <- 0 to n
    do println(i + j)

State(10, 1, 2).inc.printAll()
