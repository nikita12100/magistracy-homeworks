package mipt.homework2

import cats.{Bifunctor, Functor}
import mipt.homework2.Decoder.Result

import scala.util.Try

trait Decoder[+E, +T]:
  def apply(raw: String): Decoder.Result[E, T]

object Decoder:

  type Result[E, T] = Either[E, T]

  def apply[E, T](using decoder: Decoder[E, T]): Decoder[E, T] = decoder

  def attempt[T](unsafe: String => T): Decoder[Throwable, T] =
    (raw: String) => Try(unsafe(raw)).toEither

  def decode[E, T](raw: String)(using decoder: Decoder[E, T]): Decoder.Result[E, T] =
    decoder(raw)

  given Bifunctor[Decoder] = new Bifunctor[Decoder]:
    override def bimap[A, B, C, D](fab: Decoder[A, B])(f: A => C, g: B => D): Decoder[C, D] =
      (data: String) => fab(data).map(g).left.map(f)

object FDecoder:

  type FDecoder[T] = Decoder[DecoderError, T]

  def decode[T](raw: String)(using decoder: FDecoder[T]): Decoder.Result[DecoderError, T] =
    decoder(raw)

  given Functor[FDecoder] = new Functor[FDecoder]:
    override def map[A, B](fa: FDecoder[A])(f: A => B): FDecoder[B] =
      (data: String) => fa(data).map(f)
