package example

import cats.data.Kleisli
import cats.implicits._

import scala.language.implicitConversions
import scala.util.Try

object KleisliSimpleEx extends App {

  sealed trait Error
  case class IntParseError(source: String) extends Error
  case class InvalidAge(age: Int) extends Error

  sealed trait Age
  case object Child extends Age
  case object Teen extends Age
  case object Adult extends Age

  type ErrorOr[A] = Either[Error, A]
  type ToErrorOr[A, B] = Kleisli[ErrorOr, A, B]

  def toFailure[A](f: Error): ErrorOr[A] = Either.left(f)
  def toSuccess[A](a: A): ErrorOr[A] = Either.right(a)


  val stringToInt: String ToErrorOr Int = Kleisli { text =>
    Try(text.toInt).fold(
      _ => toFailure(IntParseError(text)),
      number => toSuccess(number)
    )
  }

  val computeAge: Int ToErrorOr Age = Kleisli {
    case age if age < 0 => toFailure(InvalidAge(age))
    case age if age < 10 => toSuccess(Child)
    case age if age < 18 => toSuccess(Teen)
    case _ => toSuccess(Adult)
  }

  val stringToAge: Kleisli[ErrorOr, String, Age] = stringToInt andThen computeAge
  val result: ErrorOr[Age] = stringToAge.run("foobar")

  stringToAge.run("foobar") mustEqual Left(IntParseError("foobar"))
  stringToAge.run("-23") mustEqual Left(InvalidAge(-23))
  stringToAge.run("3") mustEqual Right(Child)
  stringToAge.run("12") mustEqual Right(Teen)
  stringToAge.run("25") mustEqual Right(Adult)

}
