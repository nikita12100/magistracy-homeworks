package mipt.homework4

import cats.MonadThrow
import cats.mtl.Ask
import mipt.homework4.UserErrors.*

case class Config(chunkSize: Int)

trait UserRepositoryDao:
  def findAll(config: Config): List[User]
  def create(name: UserName, age: Age, friends: Set[UserId] = Set.empty)(config: Config): Either[UserAlreadyExists, User]
  def delete(userId: UserId)(config: Config): Either[UserDoesNotExists, Unit]
  def update(user: User)(config: Config): Either[UserDoesNotExists, Unit]

trait UserRepository[F[_]]:
  def findAll: F[List[User]]
  def create(name: UserName, age: Age, friends: Set[UserId] = Set.empty): F[User]
  def delete(userId: UserId): F[Unit]
  def update(user: User): F[Unit]

object UserRepositoryDao:
  def apply[F[_]: MonadThrow](dao: UserRepositoryDao)(using Ask[F, Config]): UserRepository[F] = new UserRepository[F]:
    override def findAll: F[List[User]] = summon[Ask[F, Config]].reader(dao.findAll)

    override def create(name: UserName, age: Age, friends: Set[UserId]): F[User] =
      summon[MonadThrow[F]].flatMap(summon[Ask[F, Config]].ask)(cfg =>
        dao.create(name, age, friends)(cfg) match
          case Right(user) => MonadThrow[F].pure(user)
          case Left(e)     => MonadThrow[F].raiseError(new RuntimeException(s"User ${e.name} already exists!"))
      )
    override def delete(userId: UserId): F[Unit] =
      summon[MonadThrow[F]].flatMap(summon[Ask[F, Config]].ask)(cfg =>
        dao.delete(userId)(cfg) match
          case Right(user) => MonadThrow[F].pure(user)
          case Left(e)     => MonadThrow[F].raiseError(new RuntimeException(s"User ${e.id} does not exists!"))
      )

    override def update(user: User): F[Unit] =
      summon[MonadThrow[F]].flatMap(summon[Ask[F, Config]].ask)(cfg =>
        dao.update(user)(cfg) match
          case Right(user) => MonadThrow[F].pure(user)
          case Left(e)     => MonadThrow[F].raiseError(new RuntimeException(s"User ${e.id} does not exists!"))
      )
