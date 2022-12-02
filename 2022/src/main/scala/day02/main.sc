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
case class R(opp: String, me: String)

val bufferedSource = Source.fromURL(getClass.getResource("/day02/input.txt"))
val rounds = bufferedSource.getLines().map(_.split(' ')).map(a => R(a(0), a(1))).toVector

bufferedSource.close

val shapeScores = Map("A" -> 1, "X" -> 1, "B" -> 2, "Y" -> 2, "C" -> 3, "Z" -> 3)
val roundScores = Map(
  R("A", "X") -> 3, R("A", "Y") -> 6, R("A", "Z") -> 0,
  R("B", "X") -> 0, R("B", "Y") -> 3, R("B", "Z") -> 6,
  R("C", "X") -> 6, R("C", "Y") -> 0, R("C", "Z") -> 3)

// part 1
rounds.map(r => shapeScores(r.me) + roundScores(r)).sum

// part 2
// X means you need to lose,
// Y means you need to end the round in a draw, and
// Z means you need to win. Good luck!"

val strategy = Map(
  // lose
  "X" -> Map("A" -> "Z", "B" -> "X", "C" -> "Y"),
  // draw
  "Y" -> Map("A" -> "X", "B" -> "Y", "C" -> "Z"),
  // win
  "Z" -> Map("A" -> "Y", "B" -> "Z", "C" -> "X")
)

rounds.map(r => shapeScores(strategy(r.me)(r.opp)) + roundScores(R(r.opp, strategy(r.me)(r.opp)))).sum
