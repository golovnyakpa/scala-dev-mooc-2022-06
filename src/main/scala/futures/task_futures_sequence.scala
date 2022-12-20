package futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.impl.Promise
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  def futureToTryFuture[A](fut: Future[A])(implicit ex: ExecutionContext): Future[Try[A]] =
    fut.transform(s => Success(s))

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence, похожую
   * на Future.sequence, но в отличии от нее, возвращающую все успешные и не
   * успешные результаты. Возвращаемое тип функции - кортеж из двух списков, в
   * левом хранятся результаты успешных выполнений, в правово результаты
   * неуспешных выполнений. Не допускается использование методов объекта Await и
   * мутабельных переменных var
   */
  /**
   * @param futures
   *   список асинхронных задач
   * @return
   *   асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val futureList: Future[List[Try[A]]] = Future.sequence(futures.map(futureToTryFuture(_)))
    futureList.map { lst =>
      lst.foldLeft((List.empty[A], List.empty[Throwable])) { case (acc, current) =>
        current match {
          case Failure(e) => (acc._1, e :: acc._2)
          case Success(v) => (v :: acc._1, acc._2)
        }
      }
    }.map(x => (x._1.reverse, x._2.reverse))
  }
}
