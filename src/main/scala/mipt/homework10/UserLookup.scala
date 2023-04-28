package mipt.homework10

import glass.{Contains, Items, Property}
import mipt.utils.Homeworks.TaskSyntax

enum Role:
  case Admin
  case User
case class UserId(id: Int)
case class User(id: UserId, role: Role, friends: Vector[UserId])

object UserLookup:
  val idLens: Contains[User, Int] = task"Реализуйте оптику, фокусирующуюся на id внутри User"(1, 1)
  val adminOptional: Property[User, Role.Admin.type] = task"Реализуйте оптику, фокусирующуюся на роли Admin у User"(1, 2)
  val friendsTraversal: Items[User, UserId] = task"Реализуйте оптику, фокусирующуюся на друзьях у User"(1, 3)
