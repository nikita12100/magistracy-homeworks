package mipt.homework3

import cats.MonadThrow
import cats.data.OptionT
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import cats.syntax.traverse.toTraverseOps
import cats.syntax.applicativeError

import scala.concurrent.Future
import scala.util.control.NoStackTrace
import cats.*
import cats.implicits.*
import mipt.homework3.UserRepository.Op

trait UserRepository[F[_]]:
  def findAll: F[List[User]]
  def create(name: UserName, age: Age, friends: Set[UserId] = Set.empty): F[User]
  def delete(userId: UserId): F[Unit]
  def update(user: User): F[Unit]

object UserRepository:
  case class UserNotFoundError(id: UserId) extends Throwable

  type Op[F[_], T] = UserRepository[F] => F[T]

  given [F[_]: MonadThrow]: MonadThrow[Op[F, *]] = new MonadThrow[Op[F, *]]:
    override def flatMap[A, B](fa: Op[F, A])(f: A => Op[F, B]): Op[F, B] = repo => fa(repo).flatMap(f(_)(repo))

    override def pure[A](x: A): Op[F, A] = _ => MonadThrow[F].pure(x)

    override def handleErrorWith[A](fa: Op[F, A])(f: Throwable => Op[F, A]): Op[F, A] = repo =>
      MonadThrow[F].handleErrorWith(fa(repo))(f.andThen(_.apply(repo)))

    override def raiseError[A](e: Throwable): Op[F, A] = _ => MonadThrow[F].raiseError(e)

    override def tailRecM[A, B](a: A)(f: A => Op[F, Either[A, B]]): Op[F, B] = repo =>
      MonadThrow[F].tailRecM(a)(f.andThen(_.apply(repo)))

  object Operations:
    def findAll[F[_]]: Op[F, List[User]] =
      _.findAll

    def create[F[_]](name: UserName, age: Age, friends: Set[UserId] = Set.empty): Op[F, User] =
      _.create(name, age, friends)

    def delete[F[_]](userId: UserId): Op[F, Unit] = _.delete(userId)

    def update[F[_]](user: User): Op[F, Unit] = _.update(user)

    /** реализуйте композитные методы, используя базовые выше
      *
      * для работы с ошибками можно использовать синтаксис из cats.syntax.applicativeError val err: Op[User] =
      * UserNotFoundError(UserId(1)).raiseError[Op, User]
      */

    /** Метод опционального поиска пользователя */
    def findMaybe[F[_]](userId: UserId)(using me: MonadThrow[F]): Op[F, Option[User]] =
      findAll.map(all => all.find(user => user.id == userId))

    /** Метод поиска пользователя. Если пользователь не найден, должна генерироваться ошибка UserNotFound */
    def find[F[_]](userId: UserId)(using me: MonadThrow[F]): Op[F, User] =
      findAll.flatMap { all =>
        all.find(_.id == userId) match
          case Some(user) => MonadThrow[Op[F, *]].pure(user)
          case None       => MonadThrow[Op[F, *]].raiseError(UserNotFoundError(userId))
      }

    /** Метод добавления друга к пользователю. */
    def addFriend[F[_]](currentUserId: UserId, friendId: UserId)(using
        me: MonadThrow[F]
    ): Op[F, User] =
      for {
        user           <- find(currentUserId)
        friend         <- find(friendId)
        _              <- update(user.copy(friends = user.friends + friend.id))
        userWithFriend <- find(currentUserId)
      } yield userWithFriend

    /** Метод удаления друга у пользователя */
    def deleteFriend[F[_]](currentUserId: UserId, friendId: UserId)(using
        me: MonadThrow[F]
    ): Op[F, User] =
      for {
        user             <- find(currentUserId)
        friend           <- find(friendId)
        userWithoutFriend = user.copy(friends = user.friends - friend.id)
        _                <- update(userWithoutFriend)
      } yield userWithoutFriend

    // I hope it's correct solution (defently more beautiful)
    def myTraverse[F[_], A, B](fa: List[A])(f: A => Op[F, B])(using me: MonadThrow[F]): Op[F, List[B]] =
      fa.foldRight(MonadThrow[Op[F, *]].pure(List.empty[B])) { (a, acc) =>
        MonadThrow[Op[F, *]].map2(f(a), acc)(_ :: _)
      }

    /** Метод получения всех друзей пользователя */
    def getUserFriends[F[_]](userId: UserId)(using
        me: MonadThrow[F]
    ): Op[F, List[User]] =
      find(userId).flatMap { user =>
        myTraverse(user.friends.toList)(find)
      }

    /** Метод получения пользователей, у которых в друзьях только взрослые пользователи */
    def getUsersWithAdultOnlyFriends[F[_]](using
        me: MonadThrow[F]
    ): Op[F, List[User]] =
      def adultCheck(friends: List[UserId]) = {
        if (friends.isEmpty)
          MonadThrow[Op[F, *]].pure(Right(true))
        else
          find(friends.head).flatMap { friend =>
            if (friend.isAdult) MonadThrow[Op[F, *]].pure(Left(friends.tail))
            else MonadThrow[Op[F, *]].pure(Right(false))
          }
      }
      def getUsers(users: List[User], usersWithAdultFriends: List[User]) = {
        if (users.isEmpty)
          MonadThrow[Op[F, *]].pure(Right(usersWithAdultFriends))
        else
          if (users.head.friends.isEmpty)
          MonadThrow[Op[F, *]].pure(Left((users.tail, usersWithAdultFriends)))
          else
            MonadThrow[Op[F, *]].tailRecM(users.head.friends.toList)(adultCheck).flatMap { isAdult =>
              if (isAdult)
                MonadThrow[Op[F, *]].pure(Left((users.tail, usersWithAdultFriends :+ users.head)))
              else
                MonadThrow[Op[F, *]].pure(Left((users.tail, usersWithAdultFriends)))
            }
      }
      findAll.flatMap( users => MonadThrow[Op[F, *]].tailRecM((users, List.empty))(getUsers.tupled))

    /** Метод удаления всех молодых пользователей */
    def deleteAllJuniorUsers[F[_]](using
        me: MonadThrow[F]
    ): Op[F, Unit] =
      def deleteJuniors(users: List[User]): Op[F, Either[List[User], Unit]] =
        if (users.isEmpty) MonadThrow[Op[F, *]].pure(Right(()))
        else if (users.head.isAdult) MonadThrow[Op[F, *]].pure(Left(users.tail))
        else delete(users.head.id).flatMap(_ => MonadThrow[Op[F, *]].pure(Left(users.tail)))

      findAll.flatMap { users => MonadThrow[Op[F, *]].tailRecM(users)(deleteJuniors) }

    /** Метод создания сообщества, где все являются друзьями друг для друга. На вход подается список атрибутов
      * пользователей из сообщества
      */
    def createCommunity[F[_]](community: List[(UserName, Age)])(using
        me: MonadThrow[F]
    ): Op[F, List[User]] =
      def createUser(usersInfo: List[(UserName, Age)], users: List[User]) = {
        if (usersInfo.isEmpty)
          MonadThrow[Op[F, *]].pure(Right(users))
        else
          create(usersInfo.head._1, usersInfo.head._2).flatMap { newUser =>
            MonadThrow[Op[F, *]].pure(Left((usersInfo.tail, users :+ newUser)))
          }
      }
      def addFriends(users: List[User], friends: Set[UserId]) = {
        if (users.isEmpty)
          MonadThrow[Op[F, *]].pure(Right(()))
        else
          val newUser = users.head.copy(friends = friends - users.head.id)
          update(newUser).flatMap { _ =>
            MonadThrow[Op[F, *]].pure(Left((users.tail, friends)))
          }
      }
      MonadThrow[Op[F, *]].tailRecM((community, List.empty))(createUser.tupled).flatMap { users =>
        MonadThrow[Op[F, *]].tailRecM((users, users.map(_.id).toSet))(addFriends.tupled).flatMap(_ => findAll)
      }
