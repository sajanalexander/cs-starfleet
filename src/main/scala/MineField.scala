package org.sajanalexander.csstarfleet

import scala.io.Source

object MineField {
  def fromFile(filename: String) = {
    val mines = Source.fromFile(filename).getLines.zipWithIndex.map(components => {
      val (line, yValue) = components

      line.zipWithIndex.flatMap(charComponents => {
        val (c, xValue) = charComponents
        c match {
          case lc if lc >= 'a' && lc <= 'z' => Some((Coordinate(xValue, yValue), UndetonatedMine((lc - 'a') + 1)))
          case uc if uc >= 'A' && uc <= 'Z' => Some((Coordinate(xValue, yValue), UndetonatedMine((uc - 'A') + 27)))
          case _ => None
        }
      }).toSeq
    }).toSeq.flatten

    new MineField(mines)
  }
}

class MineField (mines: Seq[(Coordinate, Mine)]) {
  def descend() = {
    new MineField(mines.map(m => (m._1, m._2.descend())))
  }

  def destroy(coordinate: Coordinate) = {
    new MineField(mines.filterNot(m => m._1 == coordinate))
  }

  def anyMissed() = {
    mines.exists(m => m._2 match {
      case MissedMine() => true
      case _ => false
    })
  }

  def toString(shipPosition: Coordinate) = {
    if (mines.isEmpty) {
      "."
    } else {
      // calculate the bounding box by calculating the min/max values with respect to ship position
      val (xMinMax, yMinMax) = mines.foldLeft[((Int, Int), (Int, Int))]((Integer.MAX_VALUE, 0), (Integer.MAX_VALUE, 0))((current, mine) => {
        val (currentX, currentY) = current
        val (coordinate, _) = mine

        val xOffset = Math.abs(coordinate.x - shipPosition.x)
        val yOffset = Math.abs(coordinate.y - shipPosition.y)

        val (xMin, xMax) = (shipPosition.x - xOffset, shipPosition.x + xOffset)
        val (yMin, yMax) = (shipPosition.y - yOffset, shipPosition.y + yOffset)

        ((Math.min(currentX._1, xMin), Math.max(currentX._2, xMax)),
         (Math.min(currentY._1, yMin), Math.max(currentY._2, yMax)))
      })


      val (xMin, xMax) = xMinMax
      val (yMin, yMax) = yMinMax
      val coordinateToMine = mines.toMap

      (yMin to yMax).map(yValue => {
        (xMin to xMax).map(xValue => {
          coordinateToMine.get(Coordinate(xValue, yValue)) match {
            case Some(mine) => mine.toString()
            case _ => "."
          }
        }).mkString
      }).mkString("\n")
    }
  }

  def middle() : Coordinate = {
    // this assumes that each direction is of an odd length, otherwise this does not give the true-midpoint.  this is only used to determine the ship's position for initial state.  It might have been better to calculate this elsewhere
    val (width, height) = mines.foldLeft[(Int, Int)]((0, 0))((current, mine) => {
      val (coordinate, _) = mine
      (Math.max(current._1, coordinate.x), Math.max(current._2, coordinate.y))
    })
    Coordinate(width / 2, height / 2)
  }

  def numberOfMines() = {
    // only count the undetonated ones
    mines.collect { case (x, y : UndetonatedMine) => y }.size
  }
}

sealed trait Mine { 
  def descend() : Mine
}
case class UndetonatedMine(depth: Int) extends Mine {
  if (depth < 1) {
    throw new RuntimeException("Depth must be greater than 0")
  }
  def descend() = {
    if (depth > 1) { UndetonatedMine(depth - 1) } else { MissedMine() }
  }

  override def toString = {
    val c: Char = depth match {
      case d if d < 27 => ('a' + (d - 1)).toChar
      case d if d < 53 => ('A' + (d - 27)).toChar
      case _ => throw new RuntimeException("Invalid depth: " + depth)
    }

    c.toString
  }
}
case class MissedMine() extends Mine {
  def descend() = { this }

  override def toString() = "*"
}
