/**
 * An instance represents a player and a column number, so that when this move
 * is applied to a Board object, this Move's player will place a piece in
 * this Move's column in the Board.
 */

class Move(var player: Player, var column: Int) {


  if (player == null) {
    throw new IllegalArgumentException("Cannot create a Move with a null player")
  }

  if (column < 0 || Board.NUM_COLS <= column) {
    throw new IllegalArgumentException("Cannot create a Move with column that " + "is not in 0..Board.NUM_COLS-1")
  }

  override def toString(): String = {
    player + " put a piece in column " + column
  }
}