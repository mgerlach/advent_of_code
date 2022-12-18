import scala.collection.mutable
import scala.io.Source

case class Vec3(x: Int, y: Int, z: Int):
  def dist(other: Vec3) = math.abs(this.x - other.x) + math.abs(this.y - other.y) + math.abs(this.z - other.z)

  def adjacent: Set[Vec3] = Set(Vec3(x - 1, y, z), Vec3(x + 1, y, z), Vec3(x, y - 1, z), Vec3(x, y + 1, z), Vec3(x, y, z - 1), Vec3(x, y, z + 1))

val bufferedSource = Source.fromURL(getClass.getResource("/day18/input.txt"))
val cubes = bufferedSource.getLines().map(_.split(",").map(_.trim.toInt)).map(l => Vec3(l(0), l(1), l(2))).toSeq

bufferedSource.close

// part 1

// free sides of a qube = 6 - number of cubes with manhattan distance 1 (surfaces adjacent)
// O(n^2) !!
val totalSurface = cubes.map(c => 6 - cubes.count(_.dist(c) == 1)).sum

// part 2

// to find enclosed droplets of air, "flood" the surrounding room:

// determine boundaries of air around cubes,
// must extend by 1 in each direction to fully enclose cubes at outer boundaries
val min = Vec3(cubes.minBy(_.x).x - 1, cubes.minBy(_.y).y - 1, cubes.minBy(_.z).z - 1)
val max = Vec3(cubes.maxBy(_.x).x + 1, cubes.maxBy(_.y).y + 1, cubes.maxBy(_.z).z + 1)

// for easier containment checking, create a set from the seq of cubes
val cubesSet = cubes.toSet

// create a set of air cubes representing the room to be flooded (air box)
val airSet = mutable.Set(
  (for
    z <- min.z to max.z
    y <- min.y to max.y
    x <- min.x to max.x
  yield Vec3(x, y, z))
    .filter(!cubesSet.contains(_)): _*) // remove actual cubes

// now flood the room by iteratively removing air cubes (=fill with fluid) from the air box and adding them to a queue;
// then, in the next steps, do the same with the air cubes adjacent to the ones on the queue
// until the queue is empty. start with the min qube created above (which must contain air and is part of the box by def.)
val q = mutable.Queue[Vec3]()
q.append(min)
airSet.remove(min)
while (q.nonEmpty)
  val c = q.removeHead()
  val adj = c.adjacent.intersect(airSet)
  q.appendAll(adj)
  adj.foreach(airSet.remove)

// now only air cubes which could not be reached by the fluid are left in the air set,
// their total surface can be calculated in the same way as for the non-air cubes in part 1.
// the calculated surface must be fully adjacent to non-air cubes (as the air droplets were not flooded)
// so we can simply subtract it from the totalSurface of non-air cubes.
val outerSurface = totalSurface - airSet.toSeq.map(c => 6 - airSet.count(_.dist(c) == 1)).sum

// fortunately there were no droplets fully surrounded by cubes inside other droplets fully surrounded by cubes,
// otherwise those would have had to be recursively flooded to find all the total surface area within the outer droplets
// that was included in part 1...
