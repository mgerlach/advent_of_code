import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day05/test_input.txt"))
val lines = bufferedSource.getLines().map(_.split(", *").toVector).toVector

bufferedSource.close
