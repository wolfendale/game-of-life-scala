import javafx.concurrent.{ScheduledService, Task}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.util.Duration
import org.gerweck.scalafx.util._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Slider
import scalafx.scene.layout.{AnchorPane, GridPane, StackPane}

object GameOfLifeApp extends JFXApp {

  private val gameSize = 75
  private val cellSize = 12
  private val speed = 100

  private val game: ObjectProperty[GameOfLife] =
    ObjectProperty[GameOfLife](GameOfLife.random(gameSize))

  /**
    * The speed slider
    */
  private val slider = new Slider {
    value  = speed
    min    = 20
    max    = 1000
    margin = Insets(20)
  }

  /**
    * The background service to update the state of the world
    */
  private val service = {

    val service = new ScheduledService[Unit] {
      override def createTask(): Task[Unit] = {
        () =>
          game.value = game.value.next
      }
    }

    service.period <== slider.value.map(d => Duration.millis(d))
    service.start()
    service
  }


  /**
    * Main JavaFX window
    */
  stage = new PrimaryStage {

    title  = "Conway's Game Of Life"
    height = gameSize * cellSize
    width  = gameSize * cellSize

    scene  = new Scene {

      onKeyPressed = (e: KeyEvent) => {
        e.getCode match {
          case KeyCode.SPACE =>
            if (service.isRunning) {
              service.cancel()
            } else {
              service.restart()
            }
          case KeyCode.BACK_SPACE =>
            if (e.isShiftDown) {
              game.value = GameOfLife.random(gameSize)
            } else {
              game.value = GameOfLife.empty(gameSize)
            }
        }
      }

      content = new StackPane {
        children = List(

          /**
            * The grid pane containing all the cells
            */
          new GridPane {
            for {
              xpos <- 0 until gameSize
              ypos <- 0 until gameSize
            } {
              val rect = Rect(xpos, ypos, cellSize, game)
              add(rect, xpos, ypos)
            }
          },

          /**
            * The pane containing the slider
            */
          new AnchorPane {

            pickOnBounds = false
            children = slider

            AnchorPane.setBottomAnchor(slider, 30)
            AnchorPane.setLeftAnchor(slider, 10)
          }
        )
      }
    }
  }
}
