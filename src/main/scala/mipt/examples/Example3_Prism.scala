package mipt.examples

import mipt.optics.{Iso, Lens, Prism}

object Question3_Prism:
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

object Example3_Prism:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  trait Item
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  @main def e3: Unit =
    val lens: Lens[User, Status] = Lens((s, a) => s.copy(meta = s.meta.copy(status = a)), _.meta.status)

    def updateUserId(id: Int, user: User): User = lens.update(user, s => s match // Needs pattern matching, too complex
      case Status.User(_) => Status.User(id)
      case _              => s)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))

    println(updateUserId(2024, user))

  @main def e3_2: Unit =
    val prism: Prism[Status, Status.User] = Prism(
      u => u: Status,
      s => s match
        case u@Status.User(_) => Right(u)
        case _                => Left(s)
    )

    val status = Status.User(2023)
    println(prism.narrow(status))
    println(prism.narrow(Status.Admin))
    println(prism.narrow(Status.Anonymous))
    println(prism.set(status, Status.User(2024)))
    println(prism.set(Status.Admin, Status.User(2024)))
    println(prism.set(Status.Anonymous, Status.User(2024)))

  @main def e3_3: Unit =
    val prismU: Prism[Status, Status.User] = Prism(
      u => u: Status,
      s => s match
        case u@Status.User(_) => Right(u)
        case _                => Left(s)
    )
    val isoUI: Iso[Status.User, Int] = Iso(_.id, Status.User.apply)
    val prism: Prism[Status, Int] = prismU.composeIsoA(isoUI)

    val status = Status.User(2023)
    println(prism.narrow(status))
    println(prism.narrow(Status.Admin))
    println(prism.narrow(Status.Anonymous))
    println(prism.set(status, 2024))
    println(prism.set(Status.Admin, 2024))
    println(prism.set(Status.Anonymous, 2024))

    /// Prism[User, Status.User] = ???
