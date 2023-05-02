package mipt.homework10

import glass.Items
import mipt.utils.Homeworks.TaskSyntax

enum Item:
  case SingleItem(name: String, price: Int)
  case Packaging(name: String, pricePerItem: Int, numberOfItems: Int)
case class WarehouseRegistry(shipped: Vector[Item], contained: Vector[Item])

object WarehouseRegistry:
  val warehouseNameOptic: Items[WarehouseRegistry, String] =
    task"""Создайте оптику, которая по WarehouseRegistry будет обращаться к name всех Item всех типов в
          shipped и contained одновременно. Внутри name нас интересует, содержит ли он префикс, в данном случае
          подстроку, заканчивающуюся на @. Если префикс есть, то мы фокусируемся на нём, а если нет - ни на чём.
          Таким образом должна получиться оптика, меняющая имена Item так, что, например, запрос .set("NEW") к предмету
          SingleItem("OLD@itemName", 10) сделает его SingleItem("NEW@itemName", 10)
        """(3, 1)
