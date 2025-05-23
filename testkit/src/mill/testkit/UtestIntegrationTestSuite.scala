package mill.testkit

import collection.mutable
import concurrent.duration.*

abstract class UtestIntegrationTestSuite extends utest.TestSuite with IntegrationTestSuite {
  protected def workspaceSourcePath: os.Path = os.Path(sys.env("MILL_TEST_RESOURCE_DIR"))
  protected def daemonMode: Boolean = sys.env("MILL_INTEGRATION_DAEMON_MODE").toBoolean

  /** Whether the Mill JARs are published locally alongside this Mill launcher */
  protected def isPackagedLauncher: Boolean =
    sys.env("MILL_INTEGRATION_IS_PACKAGED_LAUNCHER").toBoolean
  protected def millExecutable: os.Path =
    os.Path(System.getenv("MILL_INTEGRATION_LAUNCHER"), os.pwd)
}

trait MillWatchTests extends UtestIntegrationTestSuite {
  val maxTestDuration: FiniteDuration = 120.seconds
  val maxInstructionDuration: FiniteDuration = 10.seconds

  def awaitCompletionMarker(tester: IntegrationTester, name: String): Unit = {
    val maxTime = System.currentTimeMillis() + maxInstructionDuration.toMillis
    while (!os.exists(tester.workspacePath / "out" / name)) {
      if (System.currentTimeMillis() > maxTime) {
        sys.error(s"awaitCompletionMarker($name) timed out")
      }
      Thread.sleep(100)
    }
  }

  case class TestBaseArgs(
    expectedOut: mutable.Buffer[String], expectedErr: mutable.Buffer[String], expectedShows: mutable.Buffer[String]
  )
  def testBase(show: Boolean)(f: TestBaseArgs => IntegrationTester.EvalResult): Unit = {
    val expectedOut = mutable.Buffer.empty[String]
    // Most of these are normal `println`s, so they go to `stdout` by
    // default unless you use `show` in which case they go to `stderr`.
    val expectedErr = if (show) mutable.Buffer.empty[String] else expectedOut
    val expectedShows0 = mutable.Buffer.empty[String]

    val res = f(TestBaseArgs(expectedOut = expectedOut, expectedErr = expectedErr, expectedShows = expectedShows0))

    val (shows, out) = res.out.linesIterator.toVector.partition(_.startsWith("\""))
    val err = res.err.linesIterator.toVector.filter(s =>
      s.startsWith("Setting up ") || s.startsWith("Running ")
    )

    assert(out == expectedOut)

    // If show is not enabled, we don't expect any of our custom prints to go to stderr
    if (show) assert(err == expectedErr)
    else assert(err.isEmpty)

    val expectedShows = expectedShows0.map('"' + _ + '"')
    if (show) assert(shows == expectedShows)
  }
}
