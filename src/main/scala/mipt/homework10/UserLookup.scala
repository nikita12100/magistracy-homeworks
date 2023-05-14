package mipt.homework10

import cats.Applicative
import cats.syntax.traverse.*
import glass.{Contains, Items, Property}
import mipt.utils.Homeworks.TaskSyntax

enum Role:
  case Admin
  case User
case class UserId(id: Int)
case class User(id: UserId, role: Role, friends: Vector[UserId])

object UserLookup:
  val idLens: Contains[User, Int] = new Contains[User, Int] :
    override def set(s: User, b: Int): User = s.copy(id = UserId(b))
    override def extract(s: User): Int = s.id.id

  val adminOptional: Property[User, Role.Admin.type] = new Property[User, Role.Admin.type]:
    override def set(s: User, b: Role.Admin.type): User = s.copy(role = b)
    override def narrow(s: User): Either[User, Role.Admin.type] = s.role match
      case admin: Role.Admin.type => Right(admin)
      case _                      => Left(s)

  val friendsTraversal: Items[User, UserId] = new Items[User, UserId]:
    override def traverse[F[+_] : Applicative](s: User)(f: UserId => F[UserId]): F[User] =
      Applicative[F].map(s.friends.traverse(f))(friendsV => s.copy(friends = friendsV))
