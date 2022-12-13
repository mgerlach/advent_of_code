import scala.collection.mutable
import scala.io.Source

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

// directions
val dr = Vec(1, 0)
val du = Vec(0, -1)
val dl = Vec(-1, 0)
val dd = Vec(0, 1)

// distance of unreachable node
val NoPath = 999999

def dijkstraDist(nodes: Map[Vec, Char], start: Vec, size: Vec): Int =

  def elevation(p: Vec): Char = nodes(p) match
    case 'E' => 'z'
    case 'S' => 'a'
    case c => c

  def neighbors(p: Vec) =
    Seq(p + dr, p + du, p + dl, p + dd)
      .filter(n => n.x >= 0 && n.x < size.x && n.y >= 0 && n.y < size.y)
      .filter(n => elevation(n) - elevation(p) <= 1)

  // init

  val distances = mutable.Map(start -> 0) // point -> distance, default NoPath
  // var predecessors = mutable.Map[Vec, Vec]() // point -> point, default none
  val nodesToVisit = mutable.Set(nodes.keySet.toSeq: _*)
  var goal: Option[Vec] = None

  while (nodesToVisit.nonEmpty && goal.isEmpty)
    val u = nodesToVisit.minBy(distances.getOrElse(_, NoPath))
    nodesToVisit.remove(u)
    if (nodes(u) == 'E')
      goal = Some(u)
    else
      neighbors(u).filter(nodesToVisit.contains).foreach(v =>
        val newDist = distances.getOrElse(u, NoPath) + 1 // Dijkstra risk is always 1 (path length)
        if (newDist < distances.getOrElse(v, NoPath))
          distances.put(v, newDist)
        // predecessors.put(v, u)
      )

  goal.flatMap(distances.get).getOrElse(NoPath)


val bufferedSource = Source.fromURL(getClass.getResource("/day12/input.txt"))
val lines = bufferedSource.getLines().toSeq
bufferedSource.close()
val size = Vec(lines.head.length, lines.length)

val nodes = Map(
  (for
    row <- lines.zipWithIndex
    col <- row._1.toSeq.zipWithIndex
  yield (Vec(col._2, row._2), col._1)): _*)

// part 1
val part1 = dijkstraDist(nodes, Vec(0, 20), size)

// part 2
val part2 = math.min(part1, nodes.iterator
  .filter((_, e) => e == 'a')
  .map((n, _) => dijkstraDist(nodes, n, size))
  .min)


