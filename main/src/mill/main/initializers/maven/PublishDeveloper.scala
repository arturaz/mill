package mill.main.initializers.maven

import scala.xml.Node

private[maven] case class PublishDeveloper(
  id: String, name: String, url: String, organization: Option[String], organizationUrl: Option[String]
) {
  def render: String =
    s"""Developer(
       |  id = ${id.renderAsString},
       |  name = ${name.renderAsString},
       |  url = ${url.renderAsString},
       |  organization = ${renderOptionAsScala(organization.map(_.renderAsString))},
       |  organizationUrl = ${renderOptionAsScala(organizationUrl.map(_.renderAsString))}
       |)""".stripMargin
}
private[maven] object PublishDeveloper {
  def parse(xml: Node): Either[String, PublishDeveloper] = {
    for {
      id <- readMaybeSingleText(xml, "id")
      name <- readMaybeSingleText(xml, "name")
      url <- readMaybeSingleText(xml, "url")
      organization <- readMaybeSingleText(xml, "organization")
      organizationUrl <- readMaybeSingleText(xml, "organizationUrl")
    } yield apply(
      id = id.getOrElse(""), name = name.getOrElse(""), url = url.getOrElse(""),
      organization = organization, organizationUrl = organizationUrl
    )
  }
}
