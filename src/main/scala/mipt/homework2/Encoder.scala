package mipt.homework2

import cats.Contravariant

trait Encoder[-T]:
  def apply(value: T): String

object Encoder:
  def encode[T](value: T)(using encoder: Encoder[T]): String =
    encoder(value)

  given Contravariant[Encoder] = new Contravariant[Encoder]:
    override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] =
      (data: B) => fa(f(data))
