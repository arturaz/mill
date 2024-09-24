package mill.main.initializers.maven

/** https://maven.apache.org/guides/introduction/introduction-to-dependency-mechanism.html#dependency-scope */
private[maven] sealed trait DependencyScope {
  def render: String = this match {
    case DependencyScope.Compile => "compile"
    case DependencyScope.Provided => "provided"
    case DependencyScope.Runtime => "runtime"
    case DependencyScope.Test => "test"
    case DependencyScope.System => "system"
    case DependencyScope.Import => "import"
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

  /** This scope is similar to provided except that you have to provide the JAR which contains it explicitly.
   * The artifact is always available and is not looked up in a repository. */
  case object System extends DependencyScope

  case object Import extends DependencyScope

  def parse(str: String): Either[String, DependencyScope] = str match {
    case "compile" => Right(Compile)
    case "provided" => Right(Provided)
    case "runtime" => Right(Runtime)
    case "test" => Right(Test)
    case "system" => Right(System)
    case "import" => Right(Import)
    case _ => Left(s"Unknown dependency scope: '$str'")
  }
}
