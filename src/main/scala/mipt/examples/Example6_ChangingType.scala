package mipt.examples

import java.util.UUID

object Question6_ChangingType:
  case class InternalUser(id: UUID, age: Int, orderCount: Int)
  case class DataBaseUser(id: String, age: Int, orderCount: Int)

  def convertUser(internal: InternalUser): DataBaseUser =
    DataBaseUser(internal.id.toString, internal.age, internal.orderCount) // Too much pointless copy-paste

object Example6_ChangingType:
  case class InternalUser(id: UUID, age: Int, orderCount: Int)
  case class DataBaseUser(id: String, age: Int, orderCount: Int)

  trait Lens4[S, T, A, B]:
    def set(s: S, b: B): T
    def get(s: S): A

    def modify(s: S, f: A => B): T = set(s, f(get(s)))

  @main def e6: Unit =
    val lens: Lens4[InternalUser, DataBaseUser, UUID, String] = new Lens4[InternalUser, DataBaseUser, UUID, String]:
      override def set(s: InternalUser, b: String): DataBaseUser = DataBaseUser(b, s.age, s.orderCount)
      override def get(s: InternalUser): UUID = s.id

    val user = InternalUser(UUID.randomUUID(), 42, 2023)
    println(lens.set(user, user.id.toString))
    println(lens.set(user, "abacaba"))
    println(lens.modify(user, _.toString))
