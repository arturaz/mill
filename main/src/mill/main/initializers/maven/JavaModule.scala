package mill.main.initializers.maven

import os.Path

import scala.xml.{Elem, Node, NodeSeq, XML}

private[maven] case class JavaModule(
  dependencyManagement: Vector[Dependency],
  dependencies: Vector[Dependency],
  pomImportDependencies: Map[POMResolvedImportDependency, JavaModule],
  properties: PropertyMap,
  compilerSettings: Vector[TemplatedString],
  publishSettings: PublishSettings
) {
  def dependenciesFor(scope: DependencyManagementScope): Iterator[Dependency] =
    dependencies.iterator.filter(_.scope == scope)

  def renderProperties: Option[String] =
    JavaModule.renderProperties(properties)

  def renderPomImport(pom: POMResolvedImportDependency): Option[String] = {
    val blocks = Vector(
      // Add some of the commonly used properties in builds.
      // https://github.com/cko/predefined_maven_properties/blob/master/README.md#built-in-properties
      JavaModule.renderProperties(
        properties ++ Map(
          "project.groupId" -> TemplatedString(pom.groupId),
          "project.artifactId" -> TemplatedString(pom.artifactId),
          "project.version" -> TemplatedString(pom.version),
          "project.basedir" -> TemplatedString(pom.projectBaseDir.toString),
        )
      ),
      renderPomImports,
      renderDependencyManagementForImportedPom,
      renderDependencies(prefix = ""),
      renderDependencies(DependencyManagementScope.Test).map { deps =>
        s"""// Converted from `test` Maven scope
           |def testIvyDeps = Agg(
           |  ${deps.indentNFL(2)}
           |)""".stripMargin
      },
    ).flatten

    renderVector(blocks, "\n\n").map { contents =>
      s"""// Converted from BOM file "${pom.groupId}:${pom.artifactId}:${pom.version}"
         |object `${pom.artifactId}` {
         |  ${contents.indentNFL(2)}
         |}
         |""".stripMargin
    }
  }

  def renderPomImports: Option[String] = {
    val blocks = pomImportDependencies.iterator.flatMap { case (dep, module) => module.renderPomImport(dep) }.toVector

    renderVector(blocks, "\n\n").map { contents =>
      s"""object POMImports {
         |  ${contents.indentNFL(2)}
         |}
         |""".stripMargin
    }
  }

  def renderDependencies(scope: DependencyManagementScope): Option[String] = {
    val deps = dependenciesFor(scope).map(_.render).toVector
    renderVector(deps, ",\n")
  }

  def renderDependencies(prefix: String): Option[String] = {
    def aggFor(scope: DependencyManagementScope): Option[String] =
      renderDependencies(scope).map { deps =>
        s"""Agg(
           |  ${deps.indentNFL(2)}
           |)
           |""".stripMargin
      }

    def blockFor(scope: DependencyManagementScope, name: String): Option[String] = {
      blockForAggs(scope, name, aggFor(scope).toVector)
    }

    def blockForAggs(scope: DependencyManagementScope, name: String, aggs: Vector[String]): Option[String] = {
      if (aggs.isEmpty) None
      else Some(
        s"""// Converted from `${scope.render}` Maven scope
           |${prefix}def $name = ${aggs.iterator.map(_.trim).mkString(" ++ ")}
           |""".stripMargin
      )
    }

    val blocks = Vector(
      blockFor(DependencyManagementScope.Compile, "ivyDeps"),
      blockFor(DependencyManagementScope.Provided, "compileIvyDeps"),
      blockFor(DependencyManagementScope.Runtime, "runIvyDeps"),
    ).flatten

    renderVector(blocks, "\n\n")
  }

  def render: String = {
    val compilerSettings = renderVector(this.compilerSettings.map(_.renderAsString(TemplatedString.ScopeName.Properties)))

    val blocks = Vector(
      Some(publishSettings.render),
      renderProperties,
      renderPomImports,
      renderDependencyManagementForModule,
      renderDependencies(prefix = "override "),
      compilerSettings.map { opts =>
        s"""override def javacOptions = Seq(
           |  ${opts.indentNFL(2)}
           |)
           |""".stripMargin
      }
    ).flatten

    val testModule =
      if (
        dependenciesFor(DependencyManagementScope.Test).exists { dep =>
          dep.groupId.resolve(properties).contains("junit") && dep.artifactId.resolve(properties).contains("junit")
        }
      ) Some("TestModule.Junit4")
      else if (
        dependenciesFor(DependencyManagementScope.Test).exists { dep =>
          dep.groupId.resolve(properties).exists(_.startsWith("org.junit.jupiter"))
        }
      ) Some("TestModule.Junit5")
      else None

    val testModuleWith = testModule.fold("")(m => s" with $m")

    val testBlocks = Vector(
      renderDependencies(DependencyManagementScope.Test).map { deps =>
        s"""// Converted from `test` Maven scope
           |override def ivyDeps = Agg(
           |  ${deps.indentNFL(2)}
           |)
           |""".stripMargin
      },
      compilerSettings.map { _ =>
        s"override def javacOptions = self.javacOptions"
      },
      if (testModule.isEmpty) Some(
        s"""override def testFramework =
           |  throw new Exception("Test framework could not be automatically converted, please see https://mill-build.org/mill/Testing_Java_Projects.html for documentation.")
           |""".stripMargin
      ) else None
    ).flatten

    s"""package build
       |
       |import mill._, javalib._, publish._
       |
       |object `package` extends RootModule with MavenModule with PublishModule { self =>
       |  ${renderVector(blocks, "\n\n").getOrElse("").indentNFL(2)}
       |
       |  object test extends MavenTests${testModuleWith} {
       |    ${renderVector(testBlocks, "\n\n").getOrElse("").indentNFL(4)}
       |  }
       |}
       |""".stripMargin
  }
}
object JavaModule {
  def readFrom(path: Path): Either[String, JavaModule] = {
    val xml = XML.loadFile(path.toIO)
    parse(path, xml)
  }

  def parse(path: Path, xml: Elem): Either[String, JavaModule] = {
    val properties = readProperties(xml)
    val compilerSettings = readCompilerSettings(xml, properties)

    for {
      dependencyManagement <- readDependencies(xml \ "dependencyManagement", Dependency.parse)
        .left.map(errors => s"Could not read <dependencyManagement> from $path:\n  ${errors.mkString("\n  ")}")
      dependencies <- readDependencies(xml, Dependency.parse)
        .left.map(errors => s"Could not read <dependencies> from $path:\n  ${errors.mkString("\n  ")}")
      pomImportDeps <-
        dependencyManagement.iterator.map(POMImportDependency.from).sequenceAsValidation
          .map(_.flatten)
          .left.map(errors => s"Could not read POM imports from $path:\n  ${errors.mkString("\n  ")}")
          .flatMap { deps =>
            deps.iterator
              .map { dep =>
                dep.resolve(properties)
                  .flatMap(resolved => readFrom(resolved.path).map((resolved, _)))
                  .left.map(error => s"Could not read POM for $dep: $error")
              }
              .sequenceAsValidation
              .left.map(errors => s"Could not read POM imports from $path:\n  ${errors.mkString("\n  ")}")
              .map(_.toMap)
          }
      publishSettings <- PublishSettings.parse(xml)
    } yield apply(
      dependencyManagement = dependencyManagement, dependencies = dependencies, pomImportDependencies = pomImportDeps,
      properties = properties, compilerSettings = compilerSettings, publishSettings = publishSettings
    )
  }

  def readProperties(xml: Elem): Map[String, TemplatedString] = {
    (xml \ "properties" \ "_")
      .iterator
      .filterNot(_.isComment)
      .map(e => (e.label, TemplatedString(e.text)))
      .toMap
  }

  def readCompilerSettings(xml: Elem, properties: Map[String, TemplatedString]): Vector[TemplatedString] = {
    val mavenPrefix = "maven.compiler."

    val fromProperties = properties
      .iterator
      .filter(_._1.startsWith(mavenPrefix))
      .flatMap { case (k, v) => Vector(TemplatedString(s"-${k.replace(mavenPrefix, "")}"), v) }
      .toVector

    val plugins =
      (xml \ "build" \ "plugins" \ "plugin" \ "configuration" \ "compilerArgs" \ "arg")
        .map(e => TemplatedString(e.text))
        .toVector

    fromProperties ++ plugins
  }

  def readDependencies[A](xml: NodeSeq, parser: Node => Either[String, A]): Either[Vector[String], Vector[A]] = {
    (xml \ "dependencies" \ "dependency")
      .iterator
      .map(parser)
      .sequenceAsValidation
  }

  def renderProperties(properties: Map[String, TemplatedString]): Option[String] = {
    if (properties.isEmpty) None
    else {
      val props = properties.iterator.map { case (k, v) =>
        s"""def `$k` = ${v.renderAsString(TemplatedString.ScopeName.Properties)}"""
      }

      Some(
        s"""object Properties {
           |  ${props.mkString("\n").indentNFL(2)}
           |}
           |""".stripMargin
      )
    }
  }
}
