import scala.io.Source

//Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
//Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.

// vector indexes
val Ore = 0
val Clay = 1
val Obs = 2
val Geode = 3

def rvec(i: Int): RVec = RVec((0 to 3).map(j => if (j == i) 1 else 0).toVector)

case class RVec(v: Vector[Int]):
  def add(other: RVec): RVec = RVec(this.v.zip(other.v).map(_ + _))

  def inc(i: Int) = RVec(v.zipWithIndex.map((r, j) => if (i == j) r + 1 else r))

  def neg = v.exists(_ < 0)

  def get(r: Int) = v(r)


case class Blueprint(id: Int, costs: Vector[RVec])

val r = "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.".r

val bufferedSource = Source.fromURL(getClass.getResource("/day19/test_input.txt"))
val blueprints = bufferedSource.getLines()
  .map(l => (for (d <- "(\\d+)".r findAllIn l) yield d).map(_.toInt).toVector)
  .map(b => Blueprint(b(0), Vector(Vector(-b(1), 0, 0, 0), Vector(-b(2), 0, 0, 0), Vector(-b(3), -b(4), 0, 0), Vector(-b(5), 0, -b(6), 0)).map(RVec)))
  .toSeq

bufferedSource.close

def evalBlueprint(b: Blueprint, mins: Int): Int =

  // return (geodes, maxGeodes)
  def search(mins: Int, maxGeodes: Int, mat: RVec, robots: RVec): (Int, Int) =
    println(s"$mins $mat $robots")
    if (mins == 0)
      (mat.get(Geode), math.max(mat.get(Geode), maxGeodes))
    else // if (cut search tree by estimating possible max and comparing to maxGeodes)
      // affordable robots
      val budget = b.costs.zipWithIndex.filter((costs, _) => !mat.add(costs).neg)

      // buy = budget.map((costs, r) => search(mins - 1, maxGeodes, mat.add(costs).add(robots), robots.inc(r)))
      // r = index of robot, costs = b.costs(r)

      // do not spend (save): search(mins - 1, maxGeodes, mat.add(robots), robots)
      // (buy :+ save).maxBy((_, maxG) => maxG)
      
    (0, 0)

  // else (0, maxGeodes)

  val (_, maxGeodes) = search(mins, 0, RVec(Vector(0, 0, 0, 0)), RVec(Vector(1, 0, 0, 0)))
  maxGeodes

evalBlueprint(blueprints.head, 24)

// part 1

//blueprints.map(b => b.id * evalBlueprint(b, 24)).sum