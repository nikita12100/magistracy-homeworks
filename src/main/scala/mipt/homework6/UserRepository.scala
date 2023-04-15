package mipt.homework6

import mipt.utils.Homeworks._
import zio.{URIO, ZIO}
import UserErrors._
import mipt.homework6.User.{UserAge, UserId, UserName}
import zio.{URIO, ZIO}

object UserRepository {

  case class Config(chunkSize: Int)

  def apply(dao: UserRepositoryDao): UserRepository =
    new UserRepository {
      override def findAll: URIO[Config, List[User]] =
        ZIO.serviceWith[Config](dao.findAll)
      override def create(
          name: UserName,
          age: UserAge,
          friends: Set[UserId] = Set.empty
      ): ZIO[Config, UserAlreadyExists, User] =
        ZIO.serviceWith[Config](dao.create(name, age, friends)).absolve
      override def delete(userId: UserId): ZIO[Config, UserDoesNotExists, Unit] =
        ZIO.serviceWith[Config](dao.delete(userId)).absolve
      override def update(user: User): ZIO[Config, UserDoesNotExists, Unit] =
        ZIO.serviceWith[Config](dao.update(user)).absolve
    }

}

import User._, UserErrors._, UserRepository._

trait UserRepository {
  def findAll: URIO[Config, List[User]]
  def create(name: UserName, age: UserAge, friends: Set[UserId] = Set.empty): ZIO[Config, UserAlreadyExists, User]
  def delete(userId: UserId): ZIO[Config, UserDoesNotExists, Unit]
  def update(user: User): ZIO[Config, UserDoesNotExists, Unit]
}
