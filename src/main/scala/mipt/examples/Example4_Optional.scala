package mipt.examples

import mipt.optics.{Iso, Lens, Optional, Prism}

object Question4_Optional:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  trait Item
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  def updateUserId(id: Int, user: User): User = user.meta.status match
    case Status.User(_) => user.copy(meta = user.meta.copy(status = Status.User(id))) // Too complex
    case _              => user

object Example4_Optional:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  trait Item
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  @main def e4: Unit =
    val optional: Optional[User, Status.User] = Optional((s, a) => s.copy(meta = s.meta.copy(status = s.meta.status match
      case Status.User(_) => a
      case _              => s.meta.status
    )), s => s.meta.status match
      case u@Status.User(_) => Right(u)
      case _                => Left(s))

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))

    println(optional.set(user, Status.User(2024)))

  @main def e4_1: Unit =
    val lens: Lens[User, Status] = Lens((s, a) => s.copy(meta = s.meta.copy(status = a)), _.meta.status)
    val prism: Prism[Status, Status.User] = Prism(
      u => u: Status,
      s => s match
        case u@Status.User(_) => Right(u)
        case _                => Left(s)
    )
    val optional: Optional[User, Status.User] = Optional.fromLens(lens).compose(Optional.fromPrism(prism))

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))

    println(optional.set(user, Status.User(2024)))

  @main def e4_2: Unit =
    val lens: Lens[User, Status] = Lens((s, a) => s.copy(meta = s.meta.copy(status = a)), _.meta.status)
    val prism: Prism[Status, Status.User] = Prism(
      u => u: Status,
      s => s match
        case u@Status.User(_) => Right(u)
        case _                => Left(s)
    )
    val optionalUU: Optional[User, Status.User] = Optional.fromLens(lens).compose(Optional.fromPrism(prism))
    val iso: Iso[Status.User, Int] = Iso(_.id, Status.User.apply)
    val optional: Optional[User, Int] = optionalUU.composeIsoA(iso)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))

    println(optional.set(user, 2024))
