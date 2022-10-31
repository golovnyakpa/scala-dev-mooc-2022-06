package module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import module4.homework.dao.entity.{Role, RoleCode, User}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db
import zio.macros.accessible
import zio.{Has, RIO, ZIO, ZLayer}

import java.sql.SQLException
import javax.sql.DataSource

@accessible
object UserService {
  type UserService = Has[Service]

  trait Service {
    def listUsers(): RIO[db.DataSource, List[User]]
    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
  }

  class Impl(userRepo: UserRepository.Service) extends Service {
    val dc = db.Ctx

    def listUsers(): RIO[db.DataSource, List[User]] =
      userRepo.list()

    private def buildUserDtoForUser(user: User): ZIO[Has[DataSource], SQLException, UserDTO] =
      userRepo.userRoles(user.typedId).map(roles => UserDTO(user, roles.toSet))

    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = {
      val users: QIO[List[User]] = userRepo.list()
      users.flatMap { users =>
        ZIO.foreach(users)(u => buildUserDtoForUser(u))
      }
    }

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] =
      dc.transaction(
        for {
          us <- userRepo.createUser(user)
          _  <- userRepo.insertRoleToUser(roleCode, user.typedId)
        } yield UserDTO(us, Set(Role(roleCode.code, roleCode.code)))
      )

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]] = {
      val users: QIO[List[User]] = userRepo.listUsersWithRole(roleCode)
      users.flatMap { users =>
        ZIO.foreach(users)(u => buildUserDtoForUser(u))
      }
    }
  }

  val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] =
    ZLayer
      .fromService[UserRepository.Service, UserService.Service](rep => new Impl(rep))
}

case class UserDTO(user: User, roles: Set[Role])
