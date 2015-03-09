import scala.collection.mutable.ArrayBuffer

/**
 * An instance represents a Solver that intelligently determines
 * Moves using the Minimax algorithm.
 */

/**
 * Constructor: an instance with player who searches to depth
 * when searching the game space for moves.
 */
class AI(private var player: Player, private var depth: Int) extends Solver {

  override def getMoves(b: Board): Array[Move] = {
    val s = new State(player, b, null)
    AI.createGameTree(s, depth)

    minimax(s)

    decideMove(s.getChildren)
  }

  private def decideMove(children: Array[State]): Array[Move] = {
    val result = new Array[Move](1)

    var bestState: State = children(0)

    for (i <- 1 until children.length) {
      if (bestState.value < children(i).value) bestState = children(i)
    }

    result(0) = bestState.lastMove
    result
  }

  /**
   * State s is a node of a game tree (i.e. the current State of the game).
   * Use the Minimax algorithm to assign a numerical value to each State of the
   * tree rooted at s, indicating how desirable that State is to this player.
   */
  def minimax(s: State): Unit = {

    if (s.children.length > 0) {

      for (c1 <- s.children) {

        if (c1.children.length == 0) {
          //May want to check for each is correct
          s.children.foreach(x => evaluateBoard(x.board))

        } else {
          minimax(c1)
          val values = Array[Int](s.children.length)
          for (i <- 0 until s.children.length) {
            values(i) = s.children(i).value
          }

          if (s.player == this.player){
            s.value = values.max
          } else {
            s.value = values.min
          }

        }

      }

    }

  }

  /**
   * Evaluate the desirability of Board b for this player
   * Precondition: b is a leaf node of the game tree (because that is most
   * effective when looking several moves into the future).
   */
  def evaluateBoard(b: Board): Int = {
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
}

object AI {

  /**
   * Generate the game tree with root s of depth d.
   * The game tree's nodes are State objects that represent the state of a game
   * and whose children are all possible States that can result from the next move.
   * <p/>
   * NOTE: this method runs in exponential time with respect to d.
   * With d around 5 or 6, it is extremely slow and will start to take a very
   * long time to run.
   * <p/>
   * Note: If s has a winner (four in a row), it should be a leaf.
   */
  def createGameTree(s: State, d: Int) {
    // Note: This method must be recursive, recurse on d,
    // which should get smaller with each recursive call
    // TODO

    if (d > 0) {
      s.initializeChildren()
      val children = s.getChildren


      for (c <- children) {
        createGameTree(c, d - 1)
      }
    }
    
    val ai = new AI(s.player, d)

  }

  /**
   * Call minimax in ai with state s.
   */
  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

