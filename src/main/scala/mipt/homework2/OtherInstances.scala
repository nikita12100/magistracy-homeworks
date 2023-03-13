package mipt.homework2

import cats.Functor

trait OtherInstances:
  type Arr[-C, +A] = C => (List[A] => C) => A

  given [C]: Functor[Arr[C, *]] = new Functor[Arr[C, *]] {
    override def map[A, B](fa: Arr[C, A])(
        f: A => B
    ): Arr[C, B] = (c: C) => (fbc: List[B] => C) => f(fa(c)(fbc.compose(_.map(f))))
  }

  given [F[_]: Functor, G[_]: Functor]: Functor[[x] =>> F[G[x]]] = new Functor[[x] =>> F[G[x]]] {
    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = Functor[F].map(fga)(Functor[G].map(_)(f))
  }
