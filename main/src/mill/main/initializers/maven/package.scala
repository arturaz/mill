package mill.main.initializers

import scala.xml.{Comment, Node, NodeSeq}

package object maven {
  type PropertyMap = Map[String, TemplatedString]

  implicit class StringExts(private val str: String) extends AnyVal {
    /** Render as "foo" or """foo "bar" baz""". */
    def renderAsString: String =
      if (str.contains('"') || str.contains('\n')) s"""""\"$str""\""""
      else s""""$str""""

    /** Indents the given string by the given number of spaces except for the first line, which is not indented. */
    def indentNFL(n: Int): String =
      str.linesIterator.mkString("\n" + " " * n)
  }

  implicit class OptionEitherExts[L, R](private val opt: Option[Either[L, R]]) extends AnyVal {
    def sequence: Either[L, Option[R]] = opt match {
      case Some(Left(err)) => Left(err)
      case Some(Right(v)) => Right(Some(v))
      case None => Right(None)
    }
  }

  implicit class IteratorEitherExts[L, R](private val iter: Iterator[Either[L, R]]) extends AnyVal {
    def sequenceAsValidation: Either[Vector[L], Vector[R]] = {
      val errorsBuilder = Vector.newBuilder[L]
      val valuesBuilder = Vector.newBuilder[R]

      iter.foreach {
        case Left(err) => errorsBuilder += err
        case Right(v) => valuesBuilder += v
      }

      val errors = errorsBuilder.result()
      if (errors.nonEmpty) Left(errors) else Right(valuesBuilder.result())
    }
  }

  implicit class NodeSeqExts(private val ns: NodeSeq) extends AnyVal {
    def singleOrError: Either[String, Node] = {
      ns.length match {
        case 0 => Left(s"Expected exactly one element, found 0")
        case 1 => Right(ns.head)
        case _ => Left(s"Expected exactly one element, found $ns")
      }
    }
  }

  implicit class NodeExts(private val n: Node) extends AnyVal {
    def isComment: Boolean = n match {
      case _: Comment => true
      case _ => false
    }
  }


  /** Takes in XML like <root><foo>bar</foo></root> and returns "bar". Fails if more than one element is found. */
  def readSingleText(xml: NodeSeq, name: String): Either[String, String] =
    readMaybeSingleText(xml, name)
      .flatMap(_.toRight(s"Expected exactly one \"$name\" element in XML, found 0 elements in $xml"))

  /** Takes in XML like <root><foo>bar</foo></root> and returns "bar". Fails if more than one element is found, returns None if no element is found */
  def readMaybeSingleText(xml: NodeSeq, name: String): Either[String, Option[String]] = {
    (xml \ name) match {
      case Seq(e) => Right(Some(e.text.trim))
      case Seq() => Right(None)
      case other => Left(s"Expected exactly one \"$name\" element in XML, found ${other.size} elements in $xml")
    }
  }

  def renderVector(vector: Vector[String], separator: String = ",\n"): Option[String] = {
    if (vector.isEmpty) None else Some(vector.mkString(separator))
  }

  def renderOptionAsScala[A](opt: Option[A]): String = opt match {
    case Some(value) => s"Some($value)"
    case None => "None"
  }
}
