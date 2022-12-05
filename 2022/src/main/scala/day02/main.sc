import scala.io.Source

enum Shape(val score: Int):
  case Rck extends Shape(1)
  case Ppr extends Shape(2)
  case Scs extends Shape(3)

import Shape.*

val S = Map("A" -> Rck, "B" -> Ppr, "C" -> Scs, "X" -> Rck, "Y" -> Ppr, "Z" -> Scs)

case class R(opp: Shape, me: Shape)

val bufferedSource = Source.fromURL(getClass.getResource("/day02/input.txt"))
val rounds = bufferedSource.getLines().map(_.split(' ')).map(a => R(S(a(0)), S(a(1)))).toVector

bufferedSource.close

val roundScores = Map(
  R(Rck, Rck) -> 3, R(Rck, Ppr) -> 6, R(Rck, Scs) -> 0,
  R(Ppr, Rck) -> 0, R(Ppr, Ppr) -> 3, R(Ppr, Scs) -> 6,
  R(Scs, Rck) -> 6, R(Scs, Ppr) -> 0, R(Scs, Scs) -> 3)

// part 1
rounds.map(r => r.me.score + roundScores(r)).sum

val strategy = Map(
  // X - lose
  S("X") -> roundScores.filter((_, s) => s == 0).map((r, _) => (r.opp, r.me)),
  // Y - draw
  S("Y") -> roundScores.filter((_, s) => s == 3).map((r, _) => (r.opp, r.me)),
  // same as
  // S("Y") -> roundScores.filter((r, _) => r.opp == r.me).map((r, _) => (r.opp, r.me)),
  // Z - win
  S("Z") -> roundScores.filter((_, s) => s == 6).map((r, _) => (r.opp, r.me))
)

rounds.map(r => strategy(r.me)(r.opp).score + roundScores(R(r.opp, strategy(r.me)(r.opp)))).sum
