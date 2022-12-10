import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day10/input.txt"))
val instructions = bufferedSource.getLines().map(_.split(" ").toVector).toVector

bufferedSource.close

// part 1
val cycles = instructions.foldLeft(Seq(1))((c, i) =>
  i(0) match
    case "noop" => c :+ c.last
    case "addx" => c :+ c.last :+ c.last + i(1).toInt
)

cycles(19) * 20 +
  cycles(59) * 60 +
  cycles(99) * 100 +
  cycles(139) * 140 +
  cycles(179) * 180 +
  cycles(219) * 220

// part 2
instructions.foldLeft(("", 1, 0))((s, i) =>
  s match
    case (scr, spr, pos) => i(0) match
      case "noop" => (
        scr + (if (Seq(spr - 1, spr, spr + 1).contains(pos)) "#" else "."),
        spr,
        (pos + 1) % 40
      )
      case "addx" => (
        scr
          + (if (Seq(spr - 1, spr, spr + 1).contains(pos)) "#" else ".")
          + (if (Seq(spr - 1, spr, spr + 1).contains((pos + 1) % 40)) "#" else "."),
        spr + i(1).toInt,
        (pos + 2) % 40
      )
) match
  case (scr, _, _) => scr.grouped(40).foreach(println)

// BACEKLHF
