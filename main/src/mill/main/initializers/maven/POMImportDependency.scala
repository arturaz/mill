package mill.main.initializers.maven

import os.{Path, SubPath}


private[maven] case class POMImportDependency(groupId: TemplatedString, artifactId: TemplatedString, version: TemplatedString) {
  def resolve(properties: Map[String, TemplatedString]): Either[String, POMResolvedImportDependency] = {
    def mapErr(err: String, propName: String, value: TemplatedString) =
      s"POM import dependency $propName contains a property reference (${value.str}) which could " +
      s"not be resolved: $err"

    for {
      groupId <- groupId.resolve(properties).left.map(mapErr(_, "groupId", groupId))
      artifactId <- artifactId.resolve(properties).left.map(mapErr(_, "artifactId", artifactId))
      version <- version.resolve(properties).left.map(mapErr(_, "version", version))
    } yield POMResolvedImportDependency(groupId = groupId, artifactId = artifactId, version = version)
  }
}
object POMImportDependency {
  def from(dep: Dependency): Either[String, Option[POMImportDependency]] = {
    if (dep.scope == DependencyManagementScope.Import && dep.type_.contains(DependencyType.POM)) {
      dep.version match {
        case Some(version) => Right(Some(apply(groupId = dep.groupId, artifactId = dep.artifactId, version = version)))
        case None => Left(s"$dep has no version")
      }
    } else
      Right(None)
  }
}

private[maven] case class POMResolvedImportDependency(groupId: String, artifactId: String, version: String) {
  def path: Path =
    projectBaseDir / s"$artifactId-$version.pom"

  def projectBaseDir: Path =
    os.home / ".m2" / "repository" / SubPath(groupId.split('.').toVector) / artifactId / version
}
