import scala.annotation.tailrec
import scala.collection.immutable.LazyList.iterate
import scala.collection.mutable
import scala.io.Source

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

  def %(other: Vec): Vec = Vec((this.x + other.x) % other.x, (this.y + other.y) % other.y)

val right = Vec(1, 0)
val down = Vec(0, 1)
val left = Vec(-1, 0)
val up = Vec(0, -1)
val none = Vec(0, 0)
val bufferedSource = Source.fromURL(getClass.getResource("/day24/test_input.txt"))
val lines = bufferedSource.getLines().toIndexedSeq

bufferedSource.close

@tailrec
def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)

val height = lines.length - 2
val width = lines(0).length - 2

val cycleLength = lcm(width, height)
val dimensions = Vec(width, height)

val startPos = Vec(0, -1)
val targetPos = Vec(lines.last.indexOf('.') - 1, height)

val blizzards = (0 until height)
  .flatMap(y => lines(y + 1)
    .zipWithIndex
    .filter((c, _) => c != '.' && c != '#')
    .map((c, x) => (Vec(x - 1, y), Seq(c match
      case '>' => right
      case 'v' => down
      case '<' => left
      case '^' => up
    ))))
  .toMap

def draw(blizzards: Map[Vec, Seq[Vec]]): Unit =
  for y <- 0 until height do
    for x <- 0 until width do
      val blzds = blizzards.getOrElse(Vec(x, y), Seq())
      print(if (blzds == Seq()) "."
      else if (blzds.length == 1)
        if (blzds.head == right) ">" else if (blzds.head == down) "v" else if (blzds.head == left) "<" else "^"
      else s"${blzds.length}")
    println
  println

//draw(blizzards)

def moveBlizzards(blizzards: Map[Vec, Seq[Vec]]) =
  blizzards.toSeq
    .flatMap((pos, blzds) => blzds.map(blzd => ((pos + blzd) % dimensions, blzd)))
    .groupBy((pos, _) => pos)
    .map((pos: Vec, blzds: Seq[(Vec, Vec)]) => (pos, blzds.map((_, blzd) => blzd))) // IDEA needs the type hints here...
    .toMap

val blizzardSituations = iterate(blizzards)(moveBlizzards).take(cycleLength).toIndexedSeq

// sanity check
// moveBlizzards(blizzardSituations.last) == blizzards

// part 1

// all positions incl. start, target
val allPositions =
  ((for
    y <- 0 until height
    x <- 0 until width
  yield Vec(x, y)) :+ startPos) :+ targetPos

// all free positions for every cycle
val allPositionsAllCycles = blizzardSituations.map(blzds => allPositions.filter(p => !blzds.contains(p)).toSet)

val nodeCount = allPositionsAllCycles.map(_.size).sum

// returns distance (minutes) from start to goal plus blizzardSituation (cycle) when reaching the goal
def dijkstraDist(nodes: IndexedSeq[Set[Vec]],
                 start: Vec,
                 goal: Vec,
                 blizzardSituation: Int,
                 searchSequence: Seq[Vec] = Seq(down, right, none, left, up)): (Int, Int) =

  def neighbors(pos: Vec): Seq[Vec] =
    searchSequence
      .map(pos + _)
      .filter(pos => pos.x >= 0 && pos.x < width
        && (pos.y >= 0 || pos.y == startPos.y && pos.x == startPos.x)
        && (pos.y < height || pos.y == targetPos.y && pos.x == targetPos.x)
      )

  // init
  val distances = nodes.indices.map(i =>
    if (i == blizzardSituation) mutable.Map(start -> 0) else mutable.Map[Vec, Int]())
  //var predecessors = mutable.Map[(Vec, Int), (Vec, Int)]
  val nodesToVisit = nodes.map(set => mutable.Set(set.toSeq: _*))
  var goalDist: Option[(Int, Int)] = None

  while (goalDist.isEmpty)
    val (u, blzds, dist) =
      nodesToVisit
        .zipWithIndex
        .flatMap((nodes, blzds) => nodes
          .filter(distances(blzds).contains)
          .map(node => (node, blzds, distances(blzds)(node))))
        .minBy((_, _, dist) => dist)

    //println(s"$u $distances $nodesToVisit")
    nodesToVisit(blzds).remove(u)
    if (u == goal)
      goalDist = Some((dist, blzds))
    else
      val nextBlzds = (blzds + 1) % nodes.length
      neighbors(u).filter(nodesToVisit(nextBlzds).contains).foreach(v =>
        val newDist = dist + 1 // Dijkstra risk is always 1 (path length)
        if (!distances(nextBlzds).contains(v) || newDist < distances(nextBlzds)(v))
          distances(nextBlzds)(v) = newDist
        //predecessors.put((v, nextBlzds), (u, blzds))
      )

  goalDist.get

val t1 = System.currentTimeMillis()
val (part1, blzds1) = dijkstraDist(allPositionsAllCycles, startPos, targetPos, 0)
s"t1: ${System.currentTimeMillis() - t1}ms"

// val (part1, blzds1) = (322, 22)

// part 2
val t2_1 = System.currentTimeMillis()
val (part2_1, blzds2_1) = dijkstraDist(allPositionsAllCycles, targetPos, startPos, blzds1, Seq(up, left, none, right, down))
s"t2_1: ${System.currentTimeMillis() - t2_1}ms"

// val (part2_1, blzds2_1) = (314, 36)

val t2_2 = System.currentTimeMillis()
val (part2_2, _) = dijkstraDist(allPositionsAllCycles, startPos, targetPos, blzds2_1)
s"t2_2: ${System.currentTimeMillis() - t2_2}ms"

// val part2_2 = 338

part1 + part2_1 + part2_2