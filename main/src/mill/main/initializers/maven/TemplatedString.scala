package mill.main.initializers.maven

import scala.annotation.tailrec

/** A string that can have a reference to a property, e.g. `${foo}` */
private[maven] case class TemplatedString(str: String) {
  override def toString: String =
    if (containsProperty) s"TemplatedString(${str.renderAsString})" else str.renderAsString

  def containsProperty: Boolean = str.contains("${")

  /** Whether or not this string contains only a property reference, e.g. `${foo}` */
  def containsOnlyProperty: Boolean = str.lastIndexOf("${") == 0 && str.last == '}'

  /** Renders this for usage inside of a Scala triple-quote string interpolation, e.g. `s"""foo ${bar}"""` */
  def renderForInsideString(scopeName: TemplatedString.ScopeName): String = {
    if (containsOnlyProperty) s"$${${renderWhenOnlyProperty(scopeName)}}"
    else if (containsProperty) withReplacedReferences(scopeName)
    else str
  }

  def withReplacedReferences(scopeName: TemplatedString.ScopeName): String =
    str.replaceAll("""\$\{([\w.]+?)}""", s"""\\$${$scopeName.`$$1`}""")

  private def strWhenOnlyProperty: String = str.drop(2).dropRight(1)

  private def renderWhenOnlyProperty(scopeName: TemplatedString.ScopeName): String = s"$scopeName.`$strWhenOnlyProperty`"

  /** If it's only a property reference return "Properties.`foo`", otherwise return a string. */
  def render(scopeName: TemplatedString.ScopeName): String = {
    if (containsOnlyProperty) renderWhenOnlyProperty(scopeName)
    else {
      val asString = withReplacedReferences(scopeName).renderAsString

      if (containsProperty) s"s$asString"
      else asString
    }
  }

  def renderAsString(scopeName: TemplatedString.ScopeName): String = render(scopeName)

  /** Tries to resolve this string to a concrete value, given a map of properties. */
  @tailrec final def resolve(properties: PropertyMap): Either[String, String] = {
    if (!containsProperty) Right(str)
    else if (!containsOnlyProperty) Left(s"Cannot (yet) resolve a string that is not a pure property: \"$str\"")
    else {
      properties.get(strWhenOnlyProperty) match {
        case None => Left(s"Could not resolve property \"$strWhenOnlyProperty\" from properties map: $properties")
        case Some(templatedString) => templatedString.resolve(properties)
      }
    }
  }
}
object TemplatedString {
  sealed trait ScopeName {
    def name: String = this match {
      case ScopeName.Properties => "Properties"
      case ScopeName.DependencyManagement => "DependencyManagement"
    }

    override def toString: String = name
  }
  object ScopeName {
    case object Properties extends ScopeName
    case object DependencyManagement extends ScopeName
  }
}
