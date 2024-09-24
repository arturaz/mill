package mill.main.initializers.maven

import scala.xml.Elem

private[maven] case class PublishSettings(
  version: String,
  description: String,
  organization: String,
  url: String,
  licenses: Vector[PublishLicense],
  versionControl: PublishVersionControl,
  developers: Vector[PublishDeveloper]
) {
  def render: String =
    s"""override def publishVersion = ${version.renderAsString}
       |
       |override def pomSettings = PomSettings(
       |  description = ${description.renderAsString},
       |  organization = ${organization.renderAsString},
       |  url = ${url.renderAsString},
       |  licenses = Seq(
       |    ${renderVector(licenses.map(_.render)).getOrElse("").indentNFL(4)}
       |  ),
       |  versionControl =
       |    ${versionControl.render.indentNFL(4)},
       |  developers = Seq(
       |    ${renderVector(developers.map(_.render)).getOrElse("").indentNFL(4)}
       |  )
       |)
       |""".stripMargin
}
private[maven] object PublishSettings {
  def parse(xml: Elem): Either[String, PublishSettings] = {
    for {
      version <- readSingleText(xml, "version")
      description <- readSingleText(xml, "description")
      organization <- readMaybeSingleText(xml \ "organization", "name").map(_.getOrElse(""))
      url <- readMaybeSingleText(xml, "url").flatMap {
        case Some(url) => Right(url)
        case None => readSingleText(xml \ "organization", "url")
      }
      licenses <- (xml \ "licenses" \ "license").map(PublishLicense.parse).iterator.sequenceAsValidation
        .left.map(errors => s"Could not read licenses:\n  ${errors.mkString("\n  ")}")
        .map(_.flatten)
      versionControl <- (xml \ "scm").singleOrError.flatMap(PublishVersionControl.parse)
      developers <- (xml \ "developers" \ "developer").map(PublishDeveloper.parse).iterator.sequenceAsValidation
        .left.map(errors => s"Could not read developers:\n  ${errors.mkString("\n  ")}")
    } yield apply(
      version = version, description = description, organization = organization, url = url,
      licenses = licenses, versionControl = versionControl, developers = developers
    )
  }
}
