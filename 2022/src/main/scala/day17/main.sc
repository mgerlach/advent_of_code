import scala.collection.immutable.LazyList.iterate
import scala.collection.immutable.{LazyList, Queue}
import scala.io.Source

val chamberW = 7

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

val bufferedSource = Source.fromURL(getClass.getResource("/day17/input.txt"))
val gasPush = bufferedSource.map(c => if (c == '>') Vec(1, 0) else Vec(-1, 0)).toIndexedSeq

bufferedSource.close

val startOffset = Vec(2, 3)
val down = Vec(0, -1)
val up = Vec(0, 1)
val initFloor = Queue((0 until chamberW).map(Vec(_, -1)): _*) // immutable!
val maxQueueSize = 100 // arbitrary, should not be too small

case class Rock(pos: Vec, size: Vec, pieces: Set[Vec]):

  def move(d: Vec): Rock = this.copy(pos + d, size, this.pieces.map(_ + d))

  def moveIfPossible(d: Vec, cave: Seq[Vec]): Rock =
    this.move(d) match
      case r: Rock if r.pos.x >= 0 && r.pos.x + r.size.x <= chamberW && !r.intersects(cave) => r
      case _ => this

  def intersects(cave: Seq[Vec]): Boolean =
    this.pieces.exists(cave.contains)

val shapes = IndexedSeq(
  //  ####
  Rock(Vec(0, 0), Vec(4, 1), Set(Vec(0, 0), Vec(1, 0), Vec(2, 0), Vec(3, 0))),
  //  .#.
  //  ###
  //  .#.
  Rock(Vec(0, 0), Vec(3, 3), Set(Vec(0, 1), Vec(1, 2), Vec(1, 1), Vec(1, 0), Vec(2, 1))),
  //  ..#
  //  ..#
  //  ###
  Rock(Vec(0, 0), Vec(3, 3), Set(Vec(0, 0), Vec(1, 0), Vec(2, 0), Vec(2, 1), Vec(2, 2))),
  //  #
  //  #
  //  #
  //  #
  Rock(Vec(0, 0), Vec(1, 4), Set(Vec(0, 0), Vec(0, 1), Vec(0, 2), Vec(0, 3))),
  //  ##
  //  ##
  Rock(Vec(0, 0), Vec(2, 2), Set(Vec(0, 0), Vec(1, 0), Vec(0, 1), Vec(1, 1)))
)

def initRock(pos: Vec, rCount: Int): Rock = shapes(rCount % shapes.length).move(pos)

def draw(maxY: Int, cave: Seq[Vec], rock: Rock): Unit =
  for y <- Range.inclusive(maxY - 1, -1, -1) do
    print('|')
    for x <- 0 until chamberW do
      print(if (cave.contains(Vec(x, y))) '#' else if (rock.pieces.contains(Vec(x, y))) '@' else '.')
    println('|')
  println("+" + "-".repeat(chamberW) + "+")

/**
 * @param start 4-tupel of: maxY - top of highest rock, r - index (counter) of last fallen rock, p - index into gas push directions array (% length),
 *              cave - modeled with a queue to keep only X most recently fallen rocks (~100 rocks pieces work fine, must not be too low!).
 *              value for starting from beginning would be (0, 0, 0, empty queue), but previous return values can also be passed
 *              to resume iteration.
 * @return see start param
 */
def iter(start: (Int, Int, Int, Queue[Vec])): LazyList[(Int, Int, Int, Queue[Vec])] =
  iterate(start)((maxY, r, p, cave) =>
    iterate((p, initRock(startOffset + Vec(0, maxY), r)))((p, rock) => (
      (p + 1) % gasPush.length, // next push index
      rock.moveIfPossible(gasPush(p), cave).move(down) // push & down
    ))
      .find((_, rock) => rock.intersects(cave)) // stop when rock falls "into" other rock
      .map((p, rock) => (p, rock.move(up))) match // move back to last valid position
      case Some((p, rock)) =>
        // draw(math.max(maxY, rock.pos.y + rock.size.y) + 5, cave, rock)
        (math.max(maxY, rock.pos.y + rock.size.y), // need to max here as rock can fall past other ones
          r + 1, // next rock (counter)
          p, // next index into gas push after last rock has fallen
          // remove obsolete rock pieces from cave model (figure no new rock will fall that far)
          (0 until cave.size - maxQueueSize).foldLeft(cave)((cave, _) => cave.dequeue match
            case (_, q) => q).enqueueAll(rock.pieces)
        )
  )

def iterator: LazyList[(Int, Int, Int, Queue[Vec])] = iter(start = (0, 0, 0, initFloor))

def simulate(rocks: Int): Int = iterator.take(rocks + 1).last match
  case (h, _, _, _) => h

// part 1

val start1 = System.currentTimeMillis()
val part1 = simulate(2022)
println(s"${System.currentTimeMillis() - start1}ms")

// part 2

// test data, 2022 rocks, h = 3068
// iterate.take(2022).foreach(println), check value of p (._3, index into gas push)
//
// At h = 25 (15 rocks), a 35 rock period starts in the p series, with h diff = 53
// (2022 - 15)/35 = 57 full periods for h = 3021
// + h = 25 from beginning = 3046
// + missing (2022 - 15)/35 division remainder -> 12 rocks into period
// e.g. at 27 rocks h = 47 - h = 25 at beginning of period -> h = 22
// = 3068

// test data, 1000000000000 rocks
// 15 rocks at beginning = 25
// + (1000000000000L-15) / 35 * 53 = 1.514.285.714.263
// + (1000000000000L-15) % 35 = 0 -> 0 addtl. rocks
// = 1514285714288

// find period begin and length
// assume shape count * gas push length as 'worst case', i.e. each falling rock only pushed once by gas

// take 3x [worst case]
val maxLength = shapes.length * gasPush.length
val P = iterator.take(3 * maxLength).map((_, _, p, _) => p).toIndexedSeq

// take [worst case] rocks from end
val maxSlice = P.slice(P.length - maxLength, P.length)

// try to find that [worst case] length sequence in first 2*[worst case] iterations
val maxSliceStarts = iterate(0)(_ + 1)
  .filter(i => P.slice(i, i + maxLength) == maxSlice)
  .take(5) // need at least 2, taking more just to be safe about the deltas
  .toIndexedSeq // 1730, 3470, 5210, ...

val pLength = maxSliceStarts(1) - maxSliceStarts(0)

// now find where it appears the 1st time (first sequence of length pLength that repeats itself)
val pStart = iterate(0)(_ + 1)
  .find(i => P.slice(i, i + pLength) == P.slice(i + pLength, i + 2 * pLength))
  .get // 207

// -> period appears the first time at 207 (starting with p = P(pStart) = 1155)

val hBegin = simulate(pStart)
val periodHDiff = simulate(pStart + pLength) - hBegin
val periodTimes = (1000000000000L - pStart) / pLength
val remainder = (1000000000000L - pStart) % pLength
val hRemainderIntoPeriod = simulate(pStart + remainder.toInt) - hBegin

val part2 =
  hBegin
    + periodTimes * periodHDiff
    + hRemainderIntoPeriod

// 1582758620701

// simplified
val hBeginAndRemainder = simulate(pStart + remainder.toInt)
val part2s = periodTimes * periodHDiff + hBeginAndRemainder