package mill.main.initializers.maven

private[maven] sealed trait DependencyType
private[maven] object DependencyType {
  case object POM extends DependencyType
  case class Other(value: String) extends DependencyType

  def parse(str: String): DependencyType = str match {
    case "pom" => POM
    case _ => Other(str)
  }
}
