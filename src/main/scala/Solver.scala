
/**
 * An instance is an entity that can supply moves for a Connect Four game.
 */

trait Solver {

  /**
   * Return this Solver's preferred Moves. If this Solver prefers one
   * Move above all others, return an array of length 1. Larger arrays
   * indicate equally preferred Moves.
   * An array of size 0 indicates that there are no possible moves.
   * Precondition: b is not null.
   */
  def getMoves(b: Board): Array[Move]
}
