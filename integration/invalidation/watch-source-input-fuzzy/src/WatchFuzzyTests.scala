package mill.integration

import mill.testkit.MillWatchTests
import utest.*

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Random

object WatchFuzzyTests extends MillWatchTests {
  /** Instruct the mill executing `--watch` to exit. */
  def exitWatch(workspacePath: os.Path) = os.write.over(workspacePath / "watchValue.txt", "exit")

  def generateTree(srcPath: os.Path, rng: Random, numFiles: Int): Vector[os.Path] = {
    val files = mutable.ArrayBuffer.empty[os.Path]

    val startNanos = System.nanoTime()
    println(s"Generating $numFiles files in $srcPath")
    for (idx <- 0 until numFiles) {
      val number = rng.nextInt(100_000)
      val numberStr = f"$number%06d".takeRight(4)
      val dir = (os.RelPath(numberStr.take(2)) / numberStr.take(4)).resolveFrom(srcPath)
      os.makeDir.all(dir)
      val file = dir / s"file-$idx.txt"
      files += file
      os.write.over(file, s"Contents of #$idx: $number")
    }
    val time = (System.nanoTime() - startNanos) / 1e9
    println(s"Generated $numFiles files in $srcPath in $time seconds")

    files.toVector
  }

  val tests: Tests = Tests {
    test("test") - integrationTest { tester =>
      import tester.*

      val rng = new Random(10)
      val files = generateTree(workspacePath / "src", rng, 100_000)
      val evalResult = Future { eval(("--watch", "mainTask"), timeout = maxTestDuration.toMillis) }
      awaitCompletionMarker(tester, "initialized0")
      awaitCompletionMarker(tester, "mainTaskRan0")

      files.iterator.take(files.length / 2).foreach { file =>
        os.write.over(file, s"Edited ${file.last}")
      }
      awaitCompletionMarker(tester, "mainTaskRan1")

      exitWatch(workspacePath)
      Await.result(evalResult, maxInstructionDuration)
    }
  }

}
