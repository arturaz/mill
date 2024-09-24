package mill.main.initializers.maven

import scala.xml.Node

private[maven] case class PublishLicense(
  id: String,
  name: String,
  url: String,
  isOsiApproved: Boolean,
  isFsfLibre: Boolean,
  distribution: String
) {
  def render: String =
    s"""License(
       |  id = ${id.renderAsString},
       |  name = ${name.renderAsString},
       |  url = ${url.renderAsString},
       |  isOsiApproved = $isOsiApproved,
       |  isFsfLibre = $isFsfLibre,
       |  distribution = ${distribution.renderAsString}
       |)
       |""".stripMargin
}
private[maven] object PublishLicense {
  def parse(xml: Node): Either[String, Option[PublishLicense]] = {
    for {
      name <- readMaybeSingleText(xml, "name")
      url <- readMaybeSingleText(xml, "url")
      distribution <- readMaybeSingleText(xml, "distribution")
    } yield name.zip(url).zip(distribution).map { case ((name, url), distribution) =>
      apply(id = name, name = name, url = url, isOsiApproved = false, isFsfLibre = false, distribution = distribution)
    }
  }
}
