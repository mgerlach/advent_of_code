import scala.collection.immutable.LazyList.iterate
import scala.io.Source
import scala.math

class Tree(val height: Int, var visible: Boolean = false):
  override def toString: String = "" + height + (if (visible) "*" else "_")

case class Vec(x: Int, y: Int):
  def +(other: Vec): Vec = Vec(this.x + other.x, this.y + other.y)

case class Grid(grid: IndexedSeq[IndexedSeq[Tree]]):
  override def toString: String =
    "\n" + grid.map(_.mkString(" ")).mkString("\n")

  def countVisible: Int = grid.map(_.count(_.visible)).sum

  val height: Int = grid.length

  val width: Int = grid.head.length

  def treeHeight(pos: Vec): Int =
    grid(pos.y)(pos.x).height

  def inBounds(pos: Vec) =
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

  // marks visible trees from start along direction given by d, returns maxHeight
  def markVisible(start: Vec, d: Vec): Int =
    iterate(start)(_ + d)
      .takeWhile(inBounds)
      .foldLeft(-1)((maxHeight, pos) =>
        if (grid(pos.y)(pos.x).height > maxHeight)
          grid(pos.y)(pos.x).visible = true
          grid(pos.y)(pos.x).height
        else
          maxHeight)

  def countVisibleTreesOnViewAxis(pos: Vec, d: Vec): Int =
    val viewAxis = iterate(pos + d)(_ + d).takeWhile(p => inBounds(p) && treeHeight(p) < treeHeight(pos))
    viewAxis.length
      // include stopping tree
      + (if (viewAxis.nonEmpty && inBounds(viewAxis.last + d)) 1 else 0)
      // include stopping tree directly next to starting pos
      + (if (viewAxis.isEmpty && inBounds(pos + d)) 1 else 0)

val bufferedSource = Source.fromURL(getClass.getResource("/day08/input.txt"))
val grid = Grid(bufferedSource.getLines().map(_.toIndexedSeq.map(c => Tree(c - 48))).toIndexedSeq)

bufferedSource.close

// part 1

// l->r and l<-r
Range(0, grid.height).foreach(y =>
  grid.markVisible(Vec(0, y), Vec(1, 0))
  grid.markVisible(Vec(grid.width - 1, y), Vec(-1, 0))
)
// top->down and bottom->up
Range(1, grid.width - 1).foreach(x =>
  grid.markVisible(Vec(x, 0), Vec(0, 1))
  grid.markVisible(Vec(x, grid.height - 1), Vec(0, -1))
)

grid.countVisible

// part 2, find inner tree with best scenic score (most trees on four view axes)
// border trees always have score 0, so we iterate X, Y only over inner coordinates
(for
  y <- 1 until grid.height - 1 // excl.
  x <- 1 until grid.width - 1 // excl.
yield Vec(x, y))
  .foldLeft(0)((maxScenicScore, pos) =>
    math.max(
      maxScenicScore,
      grid.countVisibleTreesOnViewAxis(pos, Vec(0, -1)) // up
        * grid.countVisibleTreesOnViewAxis(pos, Vec(-1, 0)) // left
        * grid.countVisibleTreesOnViewAxis(pos, Vec(1, 0)) // right
        * grid.countVisibleTreesOnViewAxis(pos, Vec(0, 1)), // down
    ))

