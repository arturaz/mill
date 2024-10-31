package mill.main.initializers.maven

/** Dependency scopes for `dependencyManagement` tag. */
private[maven] sealed trait DependencyManagementScope {
  def render: String = this match {
    case DependencyManagementScope.Compile => "compile"
    case DependencyManagementScope.Provided => "provided"
    case DependencyManagementScope.Runtime => "runtime"
    case DependencyManagementScope.Test => "test"
    case DependencyManagementScope.Import => "import"
  }
}
private[maven] object DependencyManagementScope {
  case object Compile extends DependencyManagementScope

  /** This is much like compile, but indicates you expect the JDK or a container to provide the dependency at
   * runtime. */
  case object Provided extends DependencyManagementScope

  /** This scope indicates that the dependency is not required for compilation, but is for execution. Maven includes
   * a dependency with this scope in the runtime and test classpaths, but not the compile classpath. */
  case object Runtime extends DependencyManagementScope

  case object Test extends DependencyManagementScope

  case object Import extends DependencyManagementScope

  /**
   * @return
   *   - Left(error)
   *   - Right(Left(warning))
   *   - Right(Right(scope))
   */
  def parse(str: String): Either[String, Either[String, DependencyManagementScope]] = str match {
    case "compile" => Right(Right(Compile))
    case "provided" => Right(Right(Provided))
    case "runtime" => Right(Right(Runtime))
    case "test" => Right(Right(Test))
    case "system" => Right(Left(s"'$str' scope is deprecated and not supported"))
    case "import" => Right(Right(Import))
    case _ => Left(s"Unknown dependency scope: '$str'")
  }
}
