package mill.main.initializers.maven

import scala.xml.Node

private[maven] case class PublishVersionControl(
  browsableRepository: Option[String],
  connection: Option[String],
  developerConnection: Option[String],
  tag: Option[String]
) {
  def render: String =
    s"""VersionControl(
       |  browsableRepository = ${renderOptionAsScala(browsableRepository.map(_.renderAsString))},
       |  connection = ${renderOptionAsScala(connection.map(_.renderAsString))},
       |  developerConnection = ${renderOptionAsScala(developerConnection.map(_.renderAsString))},
       |  tag = ${renderOptionAsScala(tag.map(_.renderAsString))}
       |)""".stripMargin
}
private[maven] object PublishVersionControl {
  def parse(xml: Node): Either[String, PublishVersionControl] = {
    for {
      browsableRepository <- readMaybeSingleText(xml, "url")
      connection <- readMaybeSingleText(xml, "connection")
      developerConnection <- readMaybeSingleText(xml, "developerConnection")
      tag <- readMaybeSingleText(xml, "tag")
    } yield apply(
      browsableRepository = browsableRepository, connection = connection, developerConnection = developerConnection,
      tag = tag
    )
  }
}
