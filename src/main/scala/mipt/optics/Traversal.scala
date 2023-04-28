package mipt.optics

import cats.{Applicative, Id, Traverse}
import cats.syntax.functor.*
import cats.syntax.traverse.*

trait Traversal[S, A]:
  def traverse[F[_]: Applicative](s: S, f: A => F[A]): F[S]

  def modify(s: S, f: A => A): S = traverse[Id](s, f)
  def set(s: S, a: A): S = modify(s, _ => a)

  def compose[B](other: Traversal[A, B]): Traversal[S, B] =
    Traversal([F[_]] => (s: S, f: B => F[B]) => (a: Applicative[F]) => traverse(s, other.traverse(_, f)(a))(a))

object Traversal:
  def apply[S, A](using Traversal[S, A]): Traversal[S, A] = summon[Traversal[S, A]]

  def apply[S, A](traverseFunc: [F[_]] => (S, A => F[A]) => Applicative[F] => F[S]): Traversal[S, A] =
    new Traversal[S, A]:
      override def traverse[F[_]: Applicative](s: S, f: A => F[A]): F[S] = traverseFunc[F](s, f)(Applicative[F])

  def fromTraverse[T[_]: Traverse, A]: Traversal[T[A], A] =
    Traversal[T[A], A]([F[_]] => (t: T[A], f: A => F[A]) => t.traverse(f))

  def fromOptional[S, A](optional: Optional[S, A]): Traversal[S, A] = new Traversal[S, A]:
    override def traverse[F[_]: Applicative](s: S, f: A => F[A]): F[S] =
      optional.narrow(s).traverse(f).map(_.fold(identity, optional.set(s, _)))
