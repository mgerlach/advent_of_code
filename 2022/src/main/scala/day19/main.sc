import scala.collection.immutable.LazyList.iterate
import scala.collection.mutable
import scala.io.Source

val Collect, NoResources = -1
val Ore = 0
val Clay = 1
val Obs = 2
val Geode = 3

case class RVec(v: IndexedSeq[Int]):
  def +(other: RVec): RVec = RVec(this.v.zip(other.v).map(_ + _))

  def -(other: RVec): RVec = RVec(this.v.zip(other.v).map(_ - _))

  def <(other: RVec) = (this - other).v.exists(_ < 0)

  // Assuming this vector represents robots (any number), indicates if for each resource required in the given costs,
  // a robot exists that mines the resource.
  def robotsCanCollectFor(costs: RVec) = this.v.zip(costs.v).forall((r, c) => c == 0 || c > 0 && r > 0)

// RVec instances representing no robot/just mining as (0, 0, 0, 0), ore robot/mining as (1, 0, 0, 0), etc.
val rvec: Map[Int, RVec] = Seq(Collect, Ore, Clay, Obs, Geode).map(i => (i, RVec(IndexedSeq(Ore, Clay, Obs, Geode).map(j => if (j == i) 1 else 0)))).toMap

case class Blueprint(id: Int, costs: Map[Int, RVec])

val bufferedSource = Source.fromURL(getClass.getResource("/day19/input.txt"))
val blueprints = bufferedSource.getLines()
  .map(l => (for (d <- "(\\d+)".r findAllIn l) yield d).map(_.toInt).toIndexedSeq)
  .map(b => Blueprint(id = b(0), costs = Map(
    Collect -> RVec(IndexedSeq(0, 0, 0, 0)),
    Ore -> RVec(IndexedSeq(b(1), 0, 0, 0)),
    Clay -> RVec(IndexedSeq(b(2), 0, 0, 0)),
    Obs -> RVec(IndexedSeq(b(3), b(4), 0, 0)),
    Geode -> RVec(IndexedSeq(b(5), 0, b(6), 0)))))
  .toSeq

bufferedSource.close

def evalBlueprint(b: Blueprint, mins: Int, cutOre: Int = 1, cutClay: Int = 4, cutObs: Int = 4) =

  def search(min: Int, nextRobot: Int, robots: RVec, res: RVec, maxGeodes: Int): (RVec, RVec, Int) =
    if (min == mins)
      (robots, res, math.max(res.v(Geode), maxGeodes))
    else if (
    // cut useless branches
    // (factors by try and error)
    // stop collecting just ore ...
      res.v(Ore) > cutOre * Seq(Obs, Clay, Geode).map(m => b.costs(m).v(Ore)).sum
        // ... or clay
        || res.v(Clay) > cutClay * b.costs(Obs).v(Clay)
        // ... or obsidian
        || res.v(Obs) > cutObs * b.costs(Geode).v(Obs)
    )
      (robots, res, maxGeodes)
    /*
    TODO try these mathematically correct rules for cutting branches:
    (https://www.reddit.com/r/adventofcode/comments/zpy5rm/2022_day_19_what_are_your_insights_and/)
    1) For any resource R that's not geode: if you already have X robots creating resource R,
    and no robot requires more than X of resource R to build,
    then you never need to build another robot mining R anymore.
    This rule is correct since you can only build one robot every minute.
    2) Note that we can do a bit better: For any resource R that's not geode:
    if you already have X robots creating resource R, a current stock of Y for that resource,
    T minutes left, and no robot requires more than Z of resource R to build, and X * T+Y >= T * Z,
    then you never need to build another robot mining R anymore.
    */
    else if (res < b.costs(nextRobot) || min == mins - 1)
    // can't build, yet (or, in the last minute, there is no point), so just collect
      search(min + 1, nextRobot, robots, res + robots, maxGeodes)
    else
      // build
      val reducedRes = res - b.costs(nextRobot) // 1. start build (spend resources)
      val newRes = reducedRes + robots // 2. existing robots mine resources
      val newRobots = robots + rvec(nextRobot) // 3. new robot ready (for next minute)
      val searchSequence =
        Seq(Geode, Obs, Clay, Ore).filter(r => newRobots.robotsCanCollectFor(b.costs(r)))
      searchSequence.tail
        .foldLeft(search(min + 1, searchSequence.head, newRobots, newRes, maxGeodes))((_, _) match
          case ((maxRobots, maxRes, maxG), r) =>
            val (maxRobotsNext, maxResNext, maxGNext) = search(min + 1, r, newRobots, newRes, maxG)
            if (maxGNext > maxG) (maxRobotsNext, maxResNext, maxGNext) else (maxRobots, maxRes, maxG))

  search(min = 0, nextRobot = Collect, robots = rvec(Ore), res = rvec(NoResources), maxGeodes = 0)

def part1 =
  blueprints.map(b =>
    val (robots, res, maxGeodes) = evalBlueprint(b, 24)
    println(s"${b.id}: $robots $res $maxGeodes")
    b.id * maxGeodes
  ).sum

// part 1

val t1 = System.currentTimeMillis()
part1
// 2160
println(s"${System.currentTimeMillis() - t1}ms")
// 2.8s .. 3.0s

// part 2

val t2_1 = System.currentTimeMillis()
val b1 = evalBlueprint(blueprints.head, 32, 1, 6, 6)
// 58
println(s"${System.currentTimeMillis() - t2_1}ms")
// 78s

val t2_2 = System.currentTimeMillis()
val b2 = evalBlueprint(blueprints(1), 32, 2)
// 10
println(s"${System.currentTimeMillis() - t2_2}ms")
// 16s

val t2_3 = System.currentTimeMillis()
val b3 = evalBlueprint(blueprints(2), 32, 2)
// 23
println(s"${System.currentTimeMillis() - t2_3}ms")
// 26s

b1._3 * b2._3 * b3._3
