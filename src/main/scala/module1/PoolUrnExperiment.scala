package module1

object PoolUrnExperiment extends App {

  // Комментарий к решению: показалось, что условие по ссылке https://www.matburo.ru/tvbook_sub.php?p=par15
  // и в условии задания чуть отличаются. По ссылке объясняется, как найти условную вероятность вытащить
  // белый шар при условии, что первый раз вытащили белый.
  // В тексте задания же просят создать класс и реализовать функцию, которая возвращает true,
  // если первый раз вытащили чёрный, а второй белый, т.е. в моём понимании это
  // чуть отличается от задачи по ссылке. По ссылке ищут (P(A|B)), а мы в такой
  // формулировке находим экспериментально P(AB). В итоге получилось, что P(AB)
  // сходится к 0.3, что сходится с теорией, т е P(AB) = (3/6) * (3/5) = 3/10.
  // Если я что-то не так понял, подскажите, пожалуйста, что сделал не так.

  def runExperimentsPool(poolSize: Int): Float = {
    val pool: List[UrnExperiment]     = List.fill(poolSize)(UrnExperiment())
    val experimentsRes: List[Boolean] = pool.map(_.runExperiment)
    val successExperimentsNum         = experimentsRes.count(identity)
    successExperimentsNum.toFloat / poolSize
  }

  println(runExperimentsPool(1000))    // 0.319
  println(runExperimentsPool(10000))   // 0.3053
  println(runExperimentsPool(100000))  // 0.29884
  println(runExperimentsPool(1000000)) // 0.300243
}
