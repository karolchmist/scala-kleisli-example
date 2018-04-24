package example

import cats.data.Kleisli
import cats.implicits._

import scala.language.implicitConversions

object KleisliEx extends App {

  sealed trait Error {
    val context: Context
  }
  case class IllegalState(context: Context) extends Error
  case class InvalidAge(context: Context, message: String) extends Error

  sealed trait Context
  case class Initial(age: Int) extends Context
  case class AgeCategory(age: Int, category: String) extends Context
  case class AgeSquared(age: Int, category: String, squared: Int) extends Context

  type Result[A] = Either[Error, A]
  type PipelineStage[A, B] = Kleisli[Result, A, B]

  def toFailure[A](f:Error): Result[A] = Either.left(f)
  def toSuccess[A](a: A): Result[A] = Either.right(a)

  val ageCategory: PipelineStage[Initial, AgeCategory] = Kleisli {
    case ctx@Initial(age) if age < 0 =>
      toFailure[AgeCategory](InvalidAge(ctx, "Age below zero"))
    case Initial(age) if age < 18 =>
      toSuccess(AgeCategory(age, category = "Teen"))
    case Initial(age) if age < 65 =>
      toSuccess(AgeCategory(age, category = "Adult"))
    case Initial(age) =>
      toSuccess(AgeCategory(age, category = "Retired"))
  }

  val printAgeCategory: PipelineStage[AgeCategory, AgeCategory] = Kleisli { ageSquared =>
      println(s"printing interesting info : $ageSquared" )
      toSuccess(ageSquared)
  }

  val ageSquared: PipelineStage[AgeCategory, AgeSquared] = Kleisli {
    case AgeCategory(age, category) =>
      toSuccess(AgeSquared(age, category, age * age))
    case ctx => toFailure(IllegalState(ctx))
  }

  val agePipeline = ageCategory andThen printAgeCategory andThen ageSquared
  println(agePipeline.run(Initial(-1)))
  println("-----")
  println(agePipeline.run(Initial(5)))
}
