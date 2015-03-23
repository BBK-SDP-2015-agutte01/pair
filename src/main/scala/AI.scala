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

  /**
   * Calculates the best next move or moves (where more than one moves are equally desirable) for this AI player
   * @param b the board on which this player must make a move.
   * @return an array containing the best next move or moves (where more than one moves are equally desirable) for this AI player.
   */
  override def getMoves(b: Board): Array[Move] = {
    val s = new State(player, b, null)
    AI.createGameTree(s, depth)
    minimax(s)
    val output = decideMove(s)
    // As the zeroth element in the returned array will be chosen as the next move for this player, the 2 lines of code
    // below can be optionally enabled to reduce the predictability of the AI player. When enabled (and more than
    // one move is equally desirable for this player) getMoves will return a random move from the array of moves,
    // determined using a random number generator.
    //
    // This means a human player cannot memorise what moves the AI will do in a given situation and then replicate that
    // situation to win over and over again.
    val random = scala.util.Random
    output(0) = output(random.nextInt(output.length))

    output
  }

  // for testing
  def getBestMoves(b: Board): Array[Move] = {
    val s = new State(player, b, null)
    AI.createGameTree(s, depth)
    minimax(s)

    val ab = new ArrayBuffer[Move]()
    for (c <- s.children) {
      if (s.value == c.value)
        ab += c.children(0).lastMove
    }
    ab.toArray
  }

   private def decideMove(s: State): Array[Move] = {
    val ab = new ArrayBuffer[Move]()
    for (c <- s.children) {
      if (s.value == c.value) ab += c.lastMove
    }
    ab.toArray
  }


  /**
   * Assigns a numerical value to each state of the tree rooted at the given
   * state object 's'. This value indicated how desirable each state is to this AI player.
   * @param s the root of the tree you wish to assign values to.
   */
  def minimax(s: State): Unit = {
    if (s.children.isEmpty){
      s.value = evaluateBoard(s.board)
    } else {
      val values = new Array[Int](s.children.length)
      for (i <- 0 until s.children.length) {
        minimax(s.children(i))
        values.update(i, s.children(i).value)
        }
      s.value = if (s.player == this.player) values.max else values.min
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
   * Generates a game tree of the given depth using ths given state object 's' as the root.
   * @param s the state you wish to create a game tree for.
   * @param d the chosen depth for the game tree you wish to create.
   */
  def createGameTree(s: State, d: Int): Unit = {
    // Note: This method must be recursive, recurse on d,
    // which should get smaller with each recursive call

    if (d == 1) s.initializeChildren()
    else if (d > 1) {
      s.initializeChildren()
      for (c <- s.children) {
        createGameTree(c, d - 1)
      }
    }
  }

  /**
   * Call minimax in ai with state s.
   */
  def minimax(ai: AI, s: State): Unit = {
    ai.minimax(s)
  }
}

