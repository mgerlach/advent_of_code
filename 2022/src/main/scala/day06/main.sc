import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day06/input.txt"))
val buffer = bufferedSource.getLines().next()
bufferedSource.close

//val buffer = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"

buffer.length

def findMarker(buffer: String, markerLen: Int): Int =
  buffer
    .sliding(markerLen)
    .zipWithIndex
    .find((window, _) => window.toSet.size == markerLen)
    .map((_, i) => i + markerLen)
    .getOrElse(-1)

// part 1
findMarker(buffer, 4)

// part 2
findMarker(buffer, 14)

// could be optimized for speed by explicitly moving the sliding window
// and checking for the positions of duplicates, skipping all steps
// that would still include the duplicates for the next window position.
