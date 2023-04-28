package mipt.examples

import cats.data.Reader
import mipt.optics.{Iso, Lens}

object Question2_Lens:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  trait Item
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  def increaseOrderCount(user: User): User =
    user.copy(meta = user.meta.copy(orderCount = user.meta.orderCount + 1)) // Too long
  def decreaseOrderCount(user: User): User = ???

object Example2_Lens:
  enum Status:
    case Admin
    case User(id: Int)
    case Anonymous
  trait Item
  case class MetaData(orderCount: Int, status: Status, boughtItems: Vector[Item])
  case class User(name: String, age: Int, meta: MetaData)

  def orderCountUpdate(update: Int => Int)(user: User): User =
    user.copy(meta = user.meta.copy(orderCount = update(user.meta.orderCount)))
  def increaseOrderCount: User => User = orderCountUpdate(_ + 1)
  def decreaseOrderCount: User => User = orderCountUpdate(_ - 1)

  @main def e2: Unit =
    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))

    println(increaseOrderCount(user))
    println(decreaseOrderCount(user))

  @main def e2_2: Unit =
    val lens = Lens[User, Int]((s, a) => s.copy(meta = s.meta.copy(orderCount = a)), s => s.meta.orderCount)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))
    println(lens.update(user, _ + 1))
    println(lens.update(user, _ - 1))

  @main def e2_3: Unit =
    val lensUM = Lens[User, MetaData]((s, a) => s.copy(meta = a), s => s.meta)
    val lensMI = Lens[MetaData, Int]((s, a) => s.copy(orderCount = a), s => s.orderCount)
    val lens = lensUM.compose(lensMI)

    val user = User("John Doe", 42, MetaData(17, Status.User(2023), Vector.empty))
    println(lens.update(user, _ + 1))
    println(lens.update(user, _ - 1))
    
  /*@main def e2_3_1: Unit =
    class Service[F[_]]:
      def ping: F[Unit]
    case class SubContext(a: Service[F])
    case class Context(a: Int, b: String, c: SubContext)
    
    given Embed[Service]
    
    type F[x] = Reader[Context, x]
    
    val context: F[Context] = Reader(identity[Context])
    val aFromSubFromContext: F[Int] = Reader(_.c.a)
    
    val lens: Lens[Context, Service[F]] = ???
    
    given [A](using Lens[Context, A]): F[A] = Reader(c => Lens[Context, A].extract(c))*/

  @main def e2_4: Unit =
    val first = Lens._1[Int, String]
    val second = Lens._2[Int, String]

    val pair = (42, "abacaba")
    println(first.extract(pair))
    println(second.extract(pair))
    println(first.set(pair, 2023))
    println(second.set(pair, "abadaba"))

  @main def e2_5: Unit =
    case class WrappedInt(value: Int)
    case class PairIS(int: Int, string: String)

    val first = Lens._1[Int, String]
    val isoI: Iso[Int, WrappedInt] = Iso(WrappedInt.apply, _.value)
    val isoP: Iso[(Int, String), PairIS] = Iso(PairIS.apply, p => (p.int, p.string))
    val lens: Lens[PairIS, WrappedInt] = first.composeIsoS(isoP).composeIsoA(isoI)

    val pair = (42, "abacaba")
    val wrappedPair = isoP.extract(pair) // PairIS(42, "abacaba")

    println(lens.extract(wrappedPair)) // gets Int from PairIS and wraps it into a WrappedInt
    println(lens.set(wrappedPair, WrappedInt(2023))) // unpacks WrappedInt and puts it into a PairIS
