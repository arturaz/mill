package mill.main.initializers.maven

import scala.xml.Node

/** Dependency for `dependencies` section of POM. */
private[maven] case class Dependency[Scope](
  groupId: TemplatedString, artifactId: TemplatedString, version: Option[TemplatedString],
  type_ : Option[DependencyType], scope: Scope
) {
  override def toString: String =
    s"Dependency(groupId=$groupId, artifactId=$artifactId, version=$version, type=$type_, scope=$scope)"
}
private[maven] object Dependency {
  def parse[Scope](xml: Node, scopeParser: String => ParseResult[Scope]): ParseResult[Dependency[Scope]] = {
    for {
      groupId <- readSingleText(xml, "groupId").map(TemplatedString.apply)
      artifactId <- readSingleText(xml, "artifactId").map(TemplatedString.apply)
      version <- readMaybeSingleText(xml, "version").map(_.map(TemplatedString.apply))
      type_ <- readMaybeSingleText(xml, "type").map(_.map(DependencyType.parse))
      scope <- readMaybeSingleText(xml, "scope").flatMap { opt =>
        opt
          .map(scopeParser(_).left.map(err => s"$err for $xml"))
          .getOrElse(Right(DependencyManagementScope.Compile))
      }
    } yield apply(groupId = groupId, artifactId = artifactId, version = version, type_ = type_, scope = scope)
  }
}
