package mipt.homework2

import mipt.homework2.domain.DegreesFahrenheit

import scala.util.{Failure, Success, Try}
import cats.implicits.toFunctorOps
import FDecoder.{FDecoder, *}
import mipt.homework2.Decoder.Result

trait OptionFDecoderInstances:
  given [T](using FDecoder[T]): FDecoder[Option[T]] = new Decoder[DecoderError, Option[T]] {
    override def apply(raw: String): Result[DecoderError, Option[T]] = raw match
      case ""       => Right(None)
      case null     => Right(None)
      case "<none>" => Right(None)
      case x        => FDecoder.decode(x).map(Some(_))
  }

trait ListFDecoderInstances:
  given [T: FDecoder]: FDecoder[List[T]] = new Decoder[DecoderError, List[T]] {
    override def apply(raw: String): Result[DecoderError, List[T]] =
      raw.split(", ").foldLeft[Decoder.Result[DecoderError, List[T]]](Right(List())) {
        case (Right(acc), s) if s.nonEmpty => decode(s).map(acc :+ _)
        case (Right(acc), _)               => Right(acc)
        case (Left(e), _)                  => Left(e)
      }
  }

object FDecoderInstances extends OptionFDecoderInstances, ListFDecoderInstances:
  given strDecoder: FDecoder[String] = new Decoder[DecoderError, String] {
    override def apply(raw: String): Result[DecoderError, String] = Right(raw)
  }

  given intDecoder: FDecoder[Int] = new Decoder[DecoderError, Int] {
    override def apply(raw: String): Result[DecoderError, Int] = Try(raw.toInt).toOption match
      case None    => Left(NumberFormatDecoderError)
      case Some(x) => Right(x)
  }

  given boolDecoder: FDecoder[Boolean] = new Decoder[DecoderError, Boolean] {
    override def apply(raw: String): Result[DecoderError, Boolean] = Try(raw.toBoolean).toOption match
      case None    => Left(IllegalArgumentDecoderError)
      case Some(x) => Right(x)
  }

  given FDecoder[DegreesFahrenheit] = new Decoder[DecoderError, DegreesFahrenheit] {
    override def apply(raw: String): Result[DecoderError, DegreesFahrenheit] = {
      decode[Int](raw).map(DegreesFahrenheit.apply).left.map(_ => InvalidDegreesFahrenheitValue)
    }
  }
