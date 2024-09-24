package mill.main.initializers.maven

import scala.xml.Node

/** Dependency for `dependencies` section of POM. */
private[maven] case class Dependency(
  groupId: TemplatedString, artifactId: TemplatedString, version: Option[TemplatedString],
  type_ : Option[DependencyType], scope: DependencyScope
) {
  override def toString: String =
    s"Dependency(groupId=$groupId, artifactId=$artifactId, version=$version, type=$type_, scope=$scope)"

  def asDependencyManagement(properties: PropertyMap): Either[String, String] = {
    for {
      groupId <- groupId.resolve(properties).left.map(err => s"Could not resolve groupId for $this: $err")
      artifactId <- artifactId.resolve(properties).left.map(err => s"Could not resolve artifactId for $this: $err")
    } yield s"$groupId:$artifactId"
  }

  def render(properties: PropertyMap): String = {
    val versionStr = version match {
      case Some(version) => version.renderForInsideString(TemplatedString.ScopeName.Properties)
      case None =>
        TemplatedString(s"$${${asDependencyManagement(properties)}")
          .renderForInsideString(TemplatedString.ScopeName.DependencyManagement)
    }

    val str = s"${groupId.renderForInsideString(TemplatedString.ScopeName.Properties)}:${
      artifactId.renderForInsideString(TemplatedString.ScopeName.Properties)}:$versionStr"
    s"ivy${str.renderAsString}"
  }
}
private[maven] object Dependency {
  def parse(xml: Node): Either[String, Dependency] = {
    for {
      groupId <- readSingleText(xml, "groupId").map(TemplatedString.apply)
      artifactId <- readSingleText(xml, "artifactId").map(TemplatedString.apply)
      version <- readMaybeSingleText(xml, "version").map(_.map(TemplatedString.apply))
      type_ <- readMaybeSingleText(xml, "type").map(_.map(DependencyType.parse))
      scope <- readMaybeSingleText(xml, "scope").flatMap { opt =>
        opt
          .map(DependencyScope.parse(_).left.map(err => s"$err for $xml"))
          .getOrElse(Right(DependencyScope.Compile))
      }
    } yield apply(groupId = groupId, artifactId = artifactId, version = version, type_ = type_, scope = scope)
  }
}
