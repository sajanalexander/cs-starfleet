package org.sajanalexander.csstarfleet

// 2d coordinate
case class Coordinate(x: Int, y: Int) {
  def applyOffset(offset: CoordinateOffset) = {
    Coordinate(x + offset.x, y + offset.y)
  }
}

// Coordinate offset, can be applied to a Coordinate to do coordinate math
case class CoordinateOffset(x: Int, y: Int)
