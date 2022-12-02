import scala.io.Source

/*
"The first column is what your opponent is going to play:
A for Rock,
B for Paper, and
C for Scissors.
The second column--" Suddenly, the Elf is called away to help with someone's tent.

The second column, you reason, must be what you should play in response:
X for Rock,
Y for Paper, and
Z for Scissors.
Winning every time would be suspicious, so the responses must have been carefully chosen.

The score for a single round is the score for the shape you selected
(1 for Rock, 2 for Paper, and 3 for Scissors)

plus the score for the outcome of the round
(0 if you lost, 3 if the round was a draw, and 6 if you won).

Rock(A,X) defeats Scissors(C,Z),
Paper(B,Y) defeats Rock(A,X)
Scissors(C,Z) defeats Paper (B,Y), and
*/
sealed trait Shape
case object Rck extends Shape
case object Ppr extends Shape
case object Scs extends Shape

val S = Map("A" -> Rck, "B" -> Ppr, "C" -> Scs, "X" -> Rck, "Y" -> Ppr, "Z" -> Scs)
case class R(opp: Shape, me: Shape)

val bufferedSource = Source.fromURL(getClass.getResource("/day02/input.txt"))
val rounds = bufferedSource.getLines().map(_.split(' ')).map(a => R(S(a(0)), S(a(1)))).toVector

bufferedSource.close

val shapeScores = Map(Rck -> 1, Ppr -> 2, Scs -> 3)
val roundScores = Map(
  R(Rck, Rck) -> 3, R(Rck, Ppr) -> 6, R(Rck, Scs) -> 0,
  R(Ppr, Rck) -> 0, R(Ppr, Ppr) -> 3, R(Ppr, Scs) -> 6,
  R(Scs, Rck) -> 6, R(Scs, Ppr) -> 0, R(Scs, Scs) -> 3)

// part 1
rounds.map(r => shapeScores(r.me) + roundScores(r)).sum

// part 2
// X (Rck) means you need to lose,
// Y (Ppr) means you need to end the round in a draw, and
// Z (Scs) means you need to win. Good luck!"

val strategy = Map(
  // lose
  Rck -> Map(Rck -> Scs, Ppr -> Rck, Scs -> Ppr),
  // draw
  Ppr -> Map(Rck -> Rck, Ppr -> Ppr, Scs -> Scs),
  // win
  Scs -> Map(Rck -> Ppr, Ppr -> Scs, Scs -> Rck)
)

rounds.map(r => shapeScores(strategy(r.me)(r.opp)) + roundScores(R(r.opp, strategy(r.me)(r.opp)))).sum
