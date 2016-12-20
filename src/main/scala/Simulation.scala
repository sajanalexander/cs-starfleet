package org.sajanalexander.csstarfleet

import scala.io.Source

object Simulation extends App {

  performSimulation(args(0), args(1))

  def performSimulation(field: String, script: String) : SimulationEvaluation = {
    val initialState = createInitialState(field)

    // fold over the script file
    val (endingState, result) = Source.fromFile(script)
          .getLines
          .zipWithIndex
          .foldLeft[(SimulationState, Option[SimulationResult])]((initialState, None))(
            (current, operationsWithIndex) => {
              val (operations, step) = operationsWithIndex
              val (currentState, currentResult) = current

              currentResult match {
                case Some(result) => {
                  result match {
                    case Cleared() => { (currentState, Some(ExtraSteps())) }
                    case _ => { current }
                  }
                }
                case _ => {
                  performSimulationStep(step, currentState, operations)
                }
              }
            })
            
    val evaluation = result match {
      case Some(res) => {
        res match {
          case PassedMine() => { Fail() }
          case ExtraSteps() => { Pass(1) }
          case Cleared() => {
            val starting = endingState.initialNumberOfMines * 10
            val shotPenalty = Math.min(endingState.volleysFired * 5,
                                       endingState.initialNumberOfMines * 5)
            val movementPenalty = Math.min(endingState.moves * 2,
                                           endingState.initialNumberOfMines * 3)

            val score = starting - shotPenalty - movementPenalty

            Pass(score)
          }
        }
      }
      case _ => { Fail() }
    }

    println()
    println(evaluation)

    evaluation
  }

  def performSimulationStep(step: Int,
                            currentState: SimulationState,
                            operationsString: String) = {
    
    println(s"Step ${step + 1}")
    println()
    println(currentState.mineField.toString(currentState.shipPosition))
    println()
    println(operationsString)

    val resultantState = (Operation.parse(operationsString) :+ Descend())
      .foldLeft[SimulationState](currentState)((current, operation) => {
        operation.perform(current)
      })
    println()
    println(resultantState.mineField.toString(resultantState.shipPosition))

    if(resultantState.mineField.anyMissed()) {
      (resultantState, Some(PassedMine()))
    } else {
      val numberOfMines = resultantState.mineField.numberOfMines()
      if (numberOfMines == 0) {
        (resultantState, Some(Cleared()))
      } else {
        (resultantState, None)
      }
    }
  }

  def createInitialState(field: String) = {
    val mineField = MineField.fromFile(field)

    SimulationState(mineField,
                    mineField.middle(),
                    mineField.numberOfMines(),
                    0,
                    0)
  }
}

case class SimulationState(mineField: MineField, 
                           shipPosition: Coordinate, 
                           initialNumberOfMines: Int,
                           volleysFired: Int,
                           moves: Int)

sealed trait SimulationResult
case class PassedMine() extends SimulationResult
case class Cleared() extends SimulationResult
case class ExtraSteps() extends SimulationResult

sealed trait SimulationEvaluation
case class Fail() extends SimulationEvaluation {
  override def toString() = "fail (0)"
}

case class Pass(score: Int) extends SimulationEvaluation {
  override def toString() = s"pass (${score})"
}
