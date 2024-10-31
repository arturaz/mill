package mill.main.initializers.maven

/** Dependency scopes for `dependencies` tag. */
private[maven] sealed trait DependencyScope {
  def render: String = this match {
    case DependencyScope.Compile => "compile"
    case DependencyScope.Provided => "provided"
    case DependencyScope.Runtime => "runtime"
    case DependencyScope.Test => "test"
  }
}
private[maven] object DependencyScope {
  case object Compile extends DependencyScope

  /** This is much like compile, but indicates you expect the JDK or a container to provide the dependency at
   * runtime. */
  case object Provided extends DependencyScope

  /** This scope indicates that the dependency is not required for compilation, but is for execution. Maven includes
   * a dependency with this scope in the runtime and test classpaths, but not the compile classpath. */
  case object Runtime extends DependencyScope

  case object Test extends DependencyScope

  /**
   * @return
   *   - Left(error)
   *   - Right(Left(warning))
   *   - Right(Right(scope))
   */
  def parse(str: String): Either[String, Either[String, DependencyScope]] = str match {
    case "compile" => Right(Right(Compile))
    case "provided" => Right(Right(Provided))
    case "runtime" => Right(Right(Runtime))
    case "test" => Right(Right(Test))
    case "system" => Right(Left(s"'$str' scope is deprecated and not supported"))
    case "import" => Right(Left(s"'$str' scope not supported for the <dependencies> section"))
    case _ => Left(s"Unknown dependency scope: '$str'")
  }
}
