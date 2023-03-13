package mipt.homework2

import mipt.homework2.domain.DegreesFahrenheit
import Decoder.*
import cats.implicits.toBifunctorOps
import scala.util.{Failure, Success, Try}

trait OptionBDecoderInstances:
  given [E, T](using decoder: Decoder[E, T]): Decoder[E, Option[T]] = new Decoder[E, Option[T]] {
    override def apply(raw: String): Result[E, Option[T]] = raw match {
      case ""          => Right(None)
      case null        => Right(None)
      case "<none>"    => Right(None)
      case raw: String => decoder(raw).map(Some(_))
    }
  }

trait ListBDecoderInstances:
  given [E, T](using decoder: Decoder[E, T]): Decoder[E, List[T]] = new Decoder[E, List[T]] {
    override def apply(raw: String): Result[E, List[T]] =
      raw.split(", ").foldLeft[Decoder.Result[E, List[T]]](Right(List())) {
        case (Right(acc), s) if s.nonEmpty => decode(s).map(acc :+ _)
        case (Right(acc), _)               => Right(acc)
        case (Left(e), _)                  => Left(e)
      }
  }

object BDecoderInstances extends OptionBDecoderInstances, ListBDecoderInstances:
  given Decoder[DecoderError, String] = Right(_)

  given Decoder[NumberFormatDecoderError.type, Int] =
    attempt(_.toInt).bimap(_ => NumberFormatDecoderError, identity)

  given Decoder[IllegalArgumentDecoderError.type, Boolean] =
    attempt(_.toBoolean).bimap(_ => IllegalArgumentDecoderError, identity)

  given Decoder[InvalidDegreesFahrenheitValue.type, DegreesFahrenheit] = (raw: String) =>
    decode[NumberFormatDecoderError.type, Int](raw).bimap(_ => InvalidDegreesFahrenheitValue, DegreesFahrenheit.apply)
