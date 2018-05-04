package example

import cats.data.Kleisli
import cats.implicits._

import scala.language.implicitConversions

object KleisliContext extends App {

  sealed trait Error
  case class IllegalState() extends Error
  case class InvalidAge(message: String) extends Error


  case class Context(age: Int, category: Option[String], squared: Option[Int]) {
    def withCategory(category: String) = copy(category = Some(category))
    def withSquared(squared: Int) = copy(squared = Some(squared))
  }

  type Result[A] = Either[Error, A]
  type PipelineStage[A, B] = Kleisli[Result, A, B]

  def toFailure[A](f:Error): Result[A] = Either.left(f)
  def toSuccess[A](a: A): Result[A] = Either.right(a)

  val ageCategory: PipelineStage[Context, Context] = Kleisli { ctx: Context =>
    val age = ctx.age
    age match {
      case _ if age < 0 => toFailure(InvalidAge("Age below zero"))
      case _ if age < 18 => toSuccess(ctx.withCategory("Teen"))
      case _ if age < 65 => toSuccess(ctx.withCategory("Adult"))
      case _ => toSuccess(ctx.withCategory("Retired"))
    }
  }

  val printAgeCategory: PipelineStage[Context, Context] = Kleisli { ctx =>
      println(s"printing interesting info : $ctx" )
      toSuccess(ctx)
  }

  val ageSquared: PipelineStage[Context, Context] = Kleisli {
    case ctx@Context(age, _, _) if age >= 0 => toSuccess(ctx.withSquared(age * age))
    case ctx => toFailure(IllegalState())
  }

  val agePipeline = ageCategory andThen printAgeCategory andThen ageSquared
  println(agePipeline.run(Context(age = -1, None, None)))
  println("-----")
  println(agePipeline.run(Context(age = 5, None, None)))
}
