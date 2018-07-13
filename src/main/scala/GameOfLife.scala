sealed trait Cell
case object Alive extends Cell
case object Dead extends Cell

case class GameOfLife(state: Seq[Seq[Cell]]) {

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
    val middle = state(y)
    val newMiddle = middle.patch(x, Seq(cell), 1)
    val newState = state.patch(y, Seq(newMiddle), 1)
    GameOfLife(newState)
  }

  def neighbours(x0: Int, y0: Int): List[Cell] = {
    for {
      x <- (x0 - 1) to (x0 + 1)
      y <- (y0 - 1) to (y0 + 1)
      if x != x0 || y != y0
    } yield get(x, y)
  }.flatten.toList
}
