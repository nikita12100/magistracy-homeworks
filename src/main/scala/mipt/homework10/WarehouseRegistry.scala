package mipt.homework10

import cats.Applicative
import cats.syntax.traverse.*
import scala.util.matching.Regex
import glass.Items
import mipt.utils.Homeworks.TaskSyntax

enum Item:
  case SingleItem(name: String, price: Int)
  case Packaging(name: String, pricePerItem: Int, numberOfItems: Int)
case class WarehouseRegistry(shipped: Vector[Item], contained: Vector[Item])

object WarehouseRegistry:
  val warehouseNameOptic: Items[WarehouseRegistry, String] = new Items[WarehouseRegistry, String] :
    override def traverse[F[+_] : Applicative](s: WarehouseRegistry)(f: String => F[String]): F[WarehouseRegistry] =
      val shipped: F[Vector[Item]] = s.shipped.traverse(transform(f))
      val contained: F[Vector[Item]] = s.contained.traverse(transform(f))
      val warehouseWithShipped = Applicative[F].ap(
        Applicative[F].pure(
          (shipped: Vector[Item]) => (contained: Vector[Item]) =>
            s.copy(
              shipped = shipped,
              contained = contained
            ))
      )(shipped)
      Applicative[F].ap(warehouseWithShipped)(contained)

    import mipt.homework10.Item.{SingleItem, Packaging}
    private def transform[F[+_] : Applicative](f: String => F[String]): Item => F[Item] = s =>
      val prefixPattern: Regex = "(.+)@(.+)".r
      val newName: F[String] = s match
          case SingleItem(prefixPattern(prefix, value), _)   => Applicative[F].map(f(prefix))(_ + "@" + value)
          case Packaging(prefixPattern(prefix, value), _, _) => Applicative[F].map(f(prefix))(_ + "@" + value)
          case SingleItem(name, _)                           => Applicative[F].pure(name)
          case Packaging(name, _, _)                         => Applicative[F].pure(name)

      Applicative[F].map(newName) { name => s match
        case singleItem: SingleItem => singleItem.copy(name = name)
        case packaging: Packaging => packaging.copy(name = name)
      }
