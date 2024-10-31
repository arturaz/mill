package mill.main.initializers.maven

sealed trait ParseResult[+A]
object ParseResult {
  /** Parsing failed because something unexpected happened. */
  case class Error(err: String) extends ParseResult[Nothing]

  /** Parsing failed because we explicitly do not support this case. */
  case class Warning(warn: String) extends ParseResult[Nothing]

  /** Parsing succeeded. */
  case class Success[A](value: A) extends ParseResult[A]

  def fromEither[A](e: Either[String, A]): ParseResult[A] = e match {
    case Left(value) => Error(value)
    case Right(value) => Success(value)
  }
}
