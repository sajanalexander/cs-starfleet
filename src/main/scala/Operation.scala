package org.sajanalexander.csstarfleet

object Operation {
  def parse(str: String) : Seq[Operation] = {
    str.trim() match {
      case e if e.isEmpty => Seq()
      case s => {
        s.split("\\s+").map(op => {
          op.toLowerCase match {
            case "north" => North()
            case "south" => South()
            case "east" => East()
            case "west" => West()
            case "alpha" => Alpha()
            case "beta" => Beta()
            case "gamma" => Gamma()
            case "delta" => Delta()
            case _ => throw new RuntimeException("Unknown operation: " + op)
          }
        }).toSeq
      }
    }
  }
}

sealed trait Operation {
  def perform(currentState: SimulationState) : SimulationState
}

case class Descend() extends Operation {
  def perform(currentState: SimulationState) = {
    currentState.copy(mineField = currentState.mineField.descend())
  }
}

sealed trait MovementOperation extends Operation {
  def offset : CoordinateOffset

  def perform(currentState: SimulationState) = {
    SimulationState(currentState.mineField,
                    currentState.shipPosition.applyOffset(offset),
                    currentState.initialNumberOfMines,
                    currentState.volleysFired,
                    currentState.moves + 1)
  }
}
case class North() extends MovementOperation {
  val offset = CoordinateOffset(0, -1)
}
case class South() extends MovementOperation {
  val offset = CoordinateOffset(0, 1)
}
case class East() extends MovementOperation {
  val offset = CoordinateOffset(1, 0)
}
case class West() extends MovementOperation {
  val offset = CoordinateOffset(-1, 0)
}

sealed trait FiringOperation extends Operation {
  def torpedoes : Seq[CoordinateOffset] 

  def perform(currentState: SimulationState) = {
    val shipPosition = currentState.shipPosition
    val newMineField = torpedoes.foldLeft[MineField](currentState.mineField)(
      (current, torpedo) => {
        current.destroy(shipPosition.applyOffset(torpedo)) 
      })
    SimulationState(newMineField,
                    shipPosition,
                    currentState.initialNumberOfMines,
                    currentState.volleysFired + 1,
                    currentState.moves)
  }
}

case class Alpha() extends FiringOperation {
  val torpedoes = Seq(CoordinateOffset(-1, -1),
                      CoordinateOffset(-1, 1),
                      CoordinateOffset(1, -1),
                      CoordinateOffset(1, 1))
}
case class Beta() extends FiringOperation {
  val torpedoes = Seq(CoordinateOffset(-1, 0),
                      CoordinateOffset(0, -1),
                      CoordinateOffset(0, 1),
                      CoordinateOffset(1, 0))
}
case class Gamma() extends FiringOperation {
  val torpedoes = Seq(CoordinateOffset(-1, 0),
                      CoordinateOffset(0, 0),
                      CoordinateOffset(1, 0))
}
case class Delta() extends FiringOperation {
  val torpedoes = Seq(CoordinateOffset(0, -1),
                      CoordinateOffset(0, 0),
                      CoordinateOffset(0, 1))
}
