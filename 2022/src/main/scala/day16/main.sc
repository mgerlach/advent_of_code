import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Valve(name: String, flowRate: Int, tunnels: Set[String])

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
val r = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z]{2}(, [A-Z]{2})*)".r

val bufferedSource = Source.fromURL(getClass.getResource("/day16/input.txt"))
val valves = bufferedSource.getLines()
  .map(r.findAllMatchIn)
  .flatMap(_.map(m => (m.group(1), Valve(m.group(1), m.group(2).toInt, m.group(3).split(", ").toSet))))
  .toMap

val closed = valves.values.filter(_.flowRate > 0).map(_.name).toSet

bufferedSource.close

def is(closed: Set[String], v: Valve) = closed.contains(v.name)

// on a valve, when "going" to the same valve (staying), it means open the valve

def planOpenIf(closed: Set[String], v: Valve) = if (is(closed, v)) List(v.name) else List()

def willOpen(v: Valve, action: String) = v.name == action

def open(closed: Set[String], v: Valve) = closed - v.name

// part 1

def search(valve: Valve, prev: Option[Valve], closed: Set[String], mins: Int, pressure: Int, maxPressure: Int): (Int, Int) =
  if (mins == 0 || closed.isEmpty)
    (pressure, math.max(maxPressure, pressure))
  else if (pressure + closed.toSeq.sortBy(-valves(_).flowRate).take(mins / 2 + 1).map(valves(_).flowRate).sum * (mins / 2 + 1) >= maxPressure)
  // It is extremely important to have a good estimation of the pressure release for any remaining valves to open here.
  // If estimated too low, the correct solution may be cut off. => Must check 1 more than remaining mins/2 valves (.take)
  // If estimated too high, calculating all tree branches may take too long.
  // At some point, not all remaining valves may be opened in time!
    (planOpenIf(closed, valve) ++ prev.map(valve.tunnels - _.name).getOrElse(valve.tunnels))
      // if valve closed, try open first, then move through tunnels
      // if prev is set we must not go back there (only possible if we first open a valve)
      .foldLeft((0, maxPressure))((_, _) match
        case ((p, maxP), action) => search(
          valves(action),
          if (willOpen(valve, action)) None else Some(valve),
          if (willOpen(valve, action)) open(closed, valve) else closed,
          mins - 1,
          pressure + (if (willOpen(valve, action)) valve.flowRate * (mins - 1) else 0),
          maxP
        ) match
          case (newP, newMaxP) => if (newP > p) (newP, newMaxP) else (p, maxP))
  else
    (0, maxPressure)

val start = System.currentTimeMillis()
search(valves("AA"), None, closed, 30, 0, 0)
println(s"${System.currentTimeMillis() - start}ms")

// part 2

def search2(valve1: Valve, valve2: Valve, prev1: Option[Valve], prev2: Option[Valve], closed: Set[String], mins: Int, pressure: Int, maxPressure: Int): (Int, Int) =
  if (mins == 0 || closed.isEmpty)
    (pressure, math.max(maxPressure, pressure))
  else if (pressure + closed.toSeq.sortBy(-valves(_).flowRate).take(mins / 2 + 1).map(valves(_).flowRate).sum * (mins / 2 + 1) > maxPressure)
  // It is extremely important to have a good estimation of the pressure release for any remaining valves to open here.
  // If estimated too low, the correct solution may be cut off.
  // (It is a bit unclear why we cannot take/multiply by (mins + 1) here, as with 2 actors, valves can be opened quicker)
  // If estimated too high, calculating all tree branches may take too long.
  // At some point, not all remaining valves may be opened in time!
    (for
    // Combine all possible actions (open valve or move) for 1 and 2!
    // If valve closed, try open first, then move through tunnels
    // If prev is set we must not go back there (only possible if we first open a valve)
      action1 <- planOpenIf(closed, valve1) ++ prev1.map(valve1.tunnels - _.name).getOrElse(valve1.tunnels)
      action2 <- planOpenIf(closed, valve2) ++ prev2.map(valve2.tunnels - _.name).getOrElse(valve2.tunnels)
    yield (action1, action2))
      // If both actors are at the same valve, let no. 2 open the valve!
      .filter((action1, _) => valve1 != valve2 || !willOpen(valve1, action1))
      .foldLeft((0, maxPressure))((_, _) match
        case ((p, maxP), (action1, action2)) => search2(
          valves(action1),
          valves(action2),
          if (willOpen(valve1, action1)) None else Some(valve1),
          if (willOpen(valve2, action2)) None else Some(valve2),
          closed
            .diff(if (willOpen(valve1, action1)) Set(action1) else Set())
            .diff(if (willOpen(valve2, action2)) Set(action2) else Set()),
          mins - 1,
          pressure
            + (if (willOpen(valve1, action1)) valve1.flowRate * (mins - 1) else 0)
            + (if (willOpen(valve2, action2)) valve2.flowRate * (mins - 1) else 0),
          maxP
        ) match
          case (newP, newMaxP) => if (newP > p) (newP, newMaxP) else (p, maxP))
  else
    (0, maxPressure)

val start2 = System.currentTimeMillis()
search2(valves("AA"), valves("AA"), None, None, closed, 26, 0, 0)
println(s"${System.currentTimeMillis() - start2}ms")
