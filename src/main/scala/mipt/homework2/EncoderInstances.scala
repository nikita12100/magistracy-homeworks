package mipt.homework2

import mipt.homework2.domain.DegreesFahrenheit

trait OptionEncoderInstances:
  given [T](using e: Encoder[T]): Encoder[Option[T]] = new Encoder[Option[T]]:
    override def apply(value: Option[T]): String = value match
      case Some(value) => e(value)
      case None        => "<none>"

trait ListEncoderInstances:
  given [T: Encoder]: Encoder[List[T]] =
    (list: List[T]) => list.map(Encoder.encode).mkString(",")

object EncoderInstances extends OptionEncoderInstances, ListEncoderInstances:
  import Encoder.given_Contravariant_Encoder
  import cats.implicits.toContravariantOps

  given Encoder[String] = identity(_)

//  given Encoder[Int] = _.toString

//  given Encoder[Boolean] = _.toString

  given [A <: AnyVal]: Encoder[A] = _.toString

  given Encoder[DegreesFahrenheit] =
    given_Encoder_A[Int].contramap { case DegreesFahrenheit(value) => value }
