package mipt.examples

import cats.syntax.traverse.*
import mipt.optics.{Lens, Optional, Traversal}

import scala.concurrent.Future

object Question5_Traversal:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  enum Item:
    case SingleItem(name: String)
    case Packaging(name: String, numberOfItems: Int)
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  def renameItemsWithPrefix(u: User, prefix: String): User =
    u.copy(meta = u.meta.copy(boughtItems = u.meta.boughtItems.map(_ match
      case Item.SingleItem(name)               => Item.SingleItem(prefix + name)
      case Item.Packaging(name, numberOfItems) => Item.Packaging(prefix + name, numberOfItems))))

object Example5_Traversal:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  enum Item:
    case SingleItem(name: String)
    case Packaging(name: String, numberOfItems: Int)
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  @main def e5: Unit =
    val lens: Lens[User, Vector[Item]] = Lens((s, a) => s.copy(meta = s.meta.copy(boughtItems = a)), _.meta.boughtItems)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector(
      Item.SingleItem("glasses"),
      Item.SingleItem("monocle"),
      Item.Packaging("magnifying glass", 15)
    )))

    val prefix = "ꙮpticsShop@"
    println(lens.update(user, _.map(_ match
      case Item.SingleItem(name)               => Item.SingleItem(prefix + name)
      case Item.Packaging(name, numberOfItems) => Item.Packaging(prefix + name, numberOfItems))))

  @main def e5_1: Unit =
    val traversal: Traversal[Vector[Item], Item] =
      Traversal[Vector[Item], Item]([F[_]] => (v: Vector[Item], f: Item => F[Item]) => v.traverse(f))

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector(
      Item.SingleItem("glasses"),
      Item.SingleItem("monocle"),
      Item.Packaging("magnifying glass", 15)
    )))

    val prefix = "ꙮpticsShop@"
    println(traversal.modify(user.meta.boughtItems, _ match
      case Item.SingleItem(name)               => Item.SingleItem(prefix + name)
      case Item.Packaging(name, numberOfItems) => Item.Packaging(prefix + name, numberOfItems)))

  @main def e5_2: Unit =
    val traversalVI: Traversal[Vector[Item], Item] = Traversal.fromTraverse
    val lens: Lens[User, Vector[Item]] = Lens((s, a) => s.copy(meta = s.meta.copy(boughtItems = a)), _.meta.boughtItems)

    val traversal: Traversal[User, Item] = Traversal.fromOptional(Optional.fromLens(lens)).compose(traversalVI)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector(
      Item.SingleItem("glasses"),
      Item.SingleItem("monocle"),
      Item.Packaging("magnifying glass", 15)
    )))

    val prefix = "ꙮpticsShop@"
    println(traversal.modify(user, _ match
      case Item.SingleItem(name)               => Item.SingleItem(prefix + name)
      case Item.Packaging(name, numberOfItems) => Item.Packaging(prefix + name, numberOfItems)))

  @main def e5_3: Unit =
    val lenIS: Lens[Item, String] = Lens((s, a) => s match
      case Item.SingleItem(_)               => Item.SingleItem(a)
      case Item.Packaging(_, numberOfItems) => Item.Packaging(a, numberOfItems)
      , _ match
      case Item.SingleItem(name)   => name
      case Item.Packaging(name, _) => name)
    val traversalVI: Traversal[Vector[Item], Item] = Traversal.fromTraverse
    val lensUV: Lens[User, Vector[Item]] =
      Lens((s, a) => s.copy(meta = s.meta.copy(boughtItems = a)), _.meta.boughtItems)

    def lensToTraversal[S, A](lens: Lens[S, A]): Traversal[S, A] = Traversal.fromOptional(Optional.fromLens(lens))

    val traversal: Traversal[User, String] =
      lensToTraversal(lensUV).compose(traversalVI).compose(lensToTraversal(lenIS))

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector(
      Item.SingleItem("glasses"),
      Item.SingleItem("monocle"),
      Item.Packaging("magnifying glass", 15)
    )))

    val prefix = "ꙮpticsShop@"
    println(traversal.modify(user, prefix + _))
