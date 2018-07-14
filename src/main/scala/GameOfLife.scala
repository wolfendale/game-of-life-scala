import scala.util.Random

sealed trait Cell
object Alive extends Cell
object Dead extends Cell

case class GameOfLife(state: Array[Array[Cell]]) {

  def next: GameOfLife = {

    val newState = state.zipWithIndex.map {
      case (row, y) =>
        row.zipWithIndex.map {
          case (Alive, x) =>
            if (neighbours(x, y).count(_ == Alive) < 2) {
              Dead
            } else if (neighbours(x, y).count(_ == Alive) > 3) {
              Dead
            } else {
              Alive
            }
          case (Dead, x) =>
            val ns = neighbours(x, y)
            if (ns.count(_ == Alive) == 3) {
              Alive
            } else {
              Dead
            }
        }
    }

    GameOfLife(newState)
  }

  def get(x: Int, y: Int): Option[Cell] =
    state.lift(y).flatMap(_.lift(x))

  def set(x: Int, y: Int, cell: Cell): GameOfLife = {
    GameOfLife {
      state.updated(y, state(y).updated(x, cell))
    }
  }

  def neighbours(x0: Int, y0: Int): List[Cell] = {
    for {
      x <- (x0 - 1) to (x0 + 1)
      y <- (y0 - 1) to (y0 + 1)
      if x != x0 || y != y0
    } yield get(x, y)
  }.flatten.toList
}

object GameOfLife {

  def empty(size: Int = 100): GameOfLife =
    GameOfLife(Array.fill(size)(Array.fill(size)(Dead)))

  def random(size: Int = 100, seed: Long = 0): GameOfLife = {

    val coords = for {
      x <- 0 until size
      y <- 0 until size
    } yield (x, y)

    if (seed != 0) {
      Random.setSeed(seed)
    }

    coords.foldLeft(GameOfLife.empty(size)) {
      case (m, (x, y)) =>
        m.set(x, y, if (Random.nextBoolean) Alive else Dead)
    }
  }
}