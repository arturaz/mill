package mill.integration

import mill.testkit.UtestIntegrationTestSuite
import utest.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

class WatchFuzzyTests extends UtestIntegrationTestSuite {
  val maxTestDuration = 120.seconds
  val maxInstructionDuration = 5.seconds

  /** Instruct the mill executing `--watch` to exit. */
  def exitWatch(workspacePath: os.Path) = os.write.over(workspacePath / "watchValue.txt", "exit")

  val tests: Tests = Tests {
    test("test") - integrationTest { tester =>
      import tester.*

      val evalResult = Future { eval(("--watch", "mainTask"), timeout = maxTestDuration.toMillis) }

      exitWatch(workspacePath)
      Await.result(evalResult, maxInstructionDuration)
    }
  }

}
