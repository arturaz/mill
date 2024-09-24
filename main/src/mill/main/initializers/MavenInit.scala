package mill.main.initializers

import mill.api.Logger
import mill.eval.Evaluator
import os.Path

object MavenInit {
  def apply(workspace: Path, rootPom: Path, log: Logger, evaluator: Evaluator, args: String*): Unit = {
    maven.JavaModule.readFrom(rootPom) match {
      case Left(error) => log.error(error)
      case Right(module) =>
        log.info(module.render)
        os.write(workspace / "build.mill", module.render)
    }
  }
}
