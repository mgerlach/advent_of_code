import scala.io.Source

val bufferedSource = Source.fromURL(getClass.getResource("/day18/test_input.txt"))
val lines = bufferedSource.getLines().map(_.split(" ").toList).toSeq

bufferedSource.close
