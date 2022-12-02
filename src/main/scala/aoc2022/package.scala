import com.google.common.base.Charsets
import com.google.common.io.Files

import java.io.File

package object aoc2022 {
  // Read file to a string
  def readFile(relPath: String): String = {
    val pwd = sys.props.get("user.dir").get
    val filePath = s"$pwd/$relPath"
    assert(new File(filePath).exists(), s"Could not find input file at: $filePath")

    Files.asCharSource(new File(filePath), Charsets.UTF_8)
      .read()
  }

  // Read a file and split by new line char
  def readFileToLines(relPath: String): Array[String] = {
    readFile(relPath).split("\n")
  }
}
