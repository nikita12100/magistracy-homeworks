package mipt.homework10

import cats.FlatMap
import cats.mtl.Ask
import glass.Contains
import mipt.utils.Homeworks.TaskSyntax

trait Embed[U[_[_]]]:
  def embed[F[_]: FlatMap](a: F[U[F]]): U[F]

object Embed:
  def apply[U[_[_]]: Embed] = summon[Embed[U]]

trait ContextOps[Context, F[_]]:
  def getContext(using Ask[F, Context]): F[Context] = Ask[F, Context].ask

  def getSubContext[A](using Ask[F, Context], Contains[Context, A]): F[A] =
    Ask[F, Context].reader(context => Contains[Context, A].extract(context))
  def getEmbedSubContext[U[_[_]]](using Ask[F, Context], Contains[Context, U[F]], Embed[U], FlatMap[F]): U[F] =
    Embed[U].embed[F](getSubContext)
