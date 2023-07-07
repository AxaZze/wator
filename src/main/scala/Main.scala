import javafx.application.Application.launch
import scalafx.Includes.*
import scalafx.scene.Scene
import scalafx.scene.layout.GridPane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.animation.AnimationTimer

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object WatorSimulation extends JFXApp3 {

  val simWidth = 100-1
  val simHeight = 100-1
  val ocean: Array[Array[Creature]] = Array.ofDim[Creature](simWidth, simHeight)

  val tunas = new ArrayBuffer[Tuna]()
  val sharks = new ArrayBuffer[Shark]()

  val tBreed = 12
  val sBreed = 10
  val sEnergy = 15

  abstract class Creature {
    var position: (Int, Int)
    var cyclesUntilReproduction: Int
  }

  class Tuna(var position: (Int, Int), var cyclesUntilReproduction: Int = tBreed) extends Creature

  class Shark(var position: (Int, Int), var cyclesUntilReproduction: Int = sBreed, var energy: Int = sEnergy) extends Creature


  def move(creature: Creature): Unit = {
    val possibleMoves = List((-1, 0), (1, 0), (0, -1), (0, 1))
      .map { case (dx, dy) => (creature.position._1 + dx, creature.position._2 + dy) }
      .filter { case (x, y) => x >= 0 && x < simWidth && y >= 0 && y < simHeight && ocean(x)(y) == null }

    if (possibleMoves.nonEmpty) {
      val newPosition = possibleMoves(Random.nextInt(possibleMoves.length))
      ocean(creature.position._1)(creature.position._2) = null
      ocean(newPosition._1)(newPosition._2) = creature
      creature.position = newPosition
    }
  }

  def simulate(): Unit = {
    val newTunas = new ArrayBuffer[Tuna]()
    val newSharks = new ArrayBuffer[Shark]()
    val deadSharks = new ArrayBuffer[Shark]()

    for (tuna <- tunas) {
      val previousPosition = tuna.position
      move(tuna)

      tuna.cyclesUntilReproduction -= 1
      if (tuna.cyclesUntilReproduction == 0) {
        tuna.cyclesUntilReproduction = tBreed
        val babyTuna = new Tuna(previousPosition)
        newTunas += babyTuna
        ocean(previousPosition._1)(previousPosition._2) = babyTuna
      }
    }

    for (shark <- sharks) {
      val previousPosition = shark.position
      val possibleFood = List((-1, 0), (1, 0), (0, -1), (0, 1))
        .map { case (dx, dy) => (shark.position._1 + dx, shark.position._2 + dy) }
        .filter { case (x, y) => x >= 0 && x < simWidth && y >= 0 && y < simHeight && ocean(x)(y).isInstanceOf[Tuna] }

      if (possibleFood.nonEmpty) {
        val foodPosition = possibleFood(Random.nextInt(possibleFood.length))
        val eatenTuna = ocean(foodPosition._1)(foodPosition._2).asInstanceOf[Tuna]
        tunas -= eatenTuna
        shark.energy += 1
        ocean(foodPosition._1)(foodPosition._2) = shark
        ocean(shark.position._1)(shark.position._2) = null
        shark.position = foodPosition
      } else {
        move(shark)
      }

      shark.cyclesUntilReproduction -= 1
      if (shark.cyclesUntilReproduction == 0) {
        shark.cyclesUntilReproduction = sBreed
        val babyShark = new Shark(previousPosition)
        newSharks += babyShark
        ocean(previousPosition._1)(previousPosition._2) = babyShark
      }

      shark.energy -= 1
      if (shark.energy == 0) {
        ocean(shark.position._1)(shark.position._2) = null
        deadSharks += shark
      }
    }

    tunas ++= newTunas
    sharks ++= newSharks
    sharks --= deadSharks
  }

  def initialize(): Unit = {
    for (_ <- 1 to 5) {
      val position = (Random.nextInt(simWidth), Random.nextInt(simHeight / 2))
      if (ocean(position._1)(position._2) == null) {
        val tuna = new Tuna(position)
        tunas += tuna
        ocean(position._1)(position._2) = tuna
      }
    }
    for (_ <- 1 to 25) {
      val position = (Random.nextInt(simWidth), Random.nextInt(simHeight / 2) + simHeight / 2)
      if (ocean(position._1)(position._2) == null) {
        val shark = new Shark(position)
        sharks += shark
        ocean(position._1)(position._2) = shark
      }
    }
  }

  val gridPane = new GridPane

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title.value = "Wator Simulation"
      scene = new Scene {
        content = gridPane
      }
    }

    def drawOcean(): Unit = {
      gridPane.children.clear()

      for (i <- 0 until simWidth; j <- 0 until simHeight) {
        val rectangle = new Rectangle {
          width = 10
          height = 10
        }

        ocean(i)(j) match {
          case null => rectangle.fill = Color.Blue
          case _: Tuna => rectangle.fill = Color.Green
          case _: Shark => rectangle.fill = Color.Red
        }

        gridPane.add(rectangle, i, j)
      }
    }

    AnimationTimer(t => {
      simulate()
      drawOcean()
    }).start()

    initialize()
    drawOcean()
  }
}
