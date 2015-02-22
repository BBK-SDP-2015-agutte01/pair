//  implicit def convertValue(v: Value): Player = v.asInstanceOf[Player]

/**
 * The two opposing players. A null Player value in board
 * indicates an empty tile.
 */

trait Player {
  def opponent: Player
}

final case object RED extends Player {
  def opponent = YELLOW
}

final case object YELLOW extends Player {
  def opponent = RED
}
