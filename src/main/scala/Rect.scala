import javafx.scene.input.MouseEvent
import javafx.scene.paint.Paint
import org.gerweck.scalafx.util._
import scalafx.beans.property.ObjectProperty
import scalafx.beans.value.ObservableValue
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

case class Rect(xpos: Int, ypos: Int, size: Int, game: ObjectProperty[GameOfLife]) extends Rectangle {

  /**
    * This sets the correct fill for each cell,
    * there's probably a better way to do this ¯\_(ツ)_/¯
    */
  val observableFill: ObservableValue[Paint, Paint] =
    game.map {
      g =>
        if (g.get(xpos, ypos).get == Alive)
          Black else White
    }

  /**
    * Sets cells on mouse press
    */
  onMousePressed = (e: MouseEvent) => {
    val cell = if (e.isShiftDown) Dead else Alive
    game.value = game.value.set(xpos, ypos, cell)
  }

  width  = size
  height = size
  fill   <== observableFill
}
