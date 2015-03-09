import java.io.{FileNotFoundException, PrintWriter, UnsupportedEncodingException}
import java.lang.IllegalArgumentException

import State.length0

import scala.beans.BeanProperty

/**
 * An instance represents the state of a game of Connect Four.
 */

/**
 * Constructor: a game State consisting of board b, with player player
 * to play next; lm is the last Move made on this game --null
 * if the user does not care what the last move made was.
 */
class State(@BeanProperty var player: Player, @BeanProperty var board: Board, @BeanProperty var lastMove: Move)
  extends Comparable[Any] {

  /**
   * All possible game States that can result from the next player's Move.
   * The length of the array equals the number of States.
   * It is an array of length 0 if there are no possible moves
   * (once it has been set; initially, it is an array of length0)
   */
  @BeanProperty
  var children: Array[State] = length0

  /**
   * How desirable this State is.
   */
  @BeanProperty
  var value: Int = 0

  /**
   * Retrieves the possible moves and initializes this State's children.
   * The result is that this State's children reflect the possible
   * States that can exist after the next move. Remember, in the
   * children it is the opposite player's turn. This method
   * initializes only this State's children; it does not recursively
   * initialize all descendants.
   */
  def initializeChildren(): Unit = {
    var opposition: Player = null

    if (player == YELLOW) opposition = RED
    else opposition = YELLOW

    var cloneBoard = new Board(board)
    // TODO add try/catch for makeMove throwing IllegalArgumentException from Board.<init>

    try {

      if (lastMove !=  null) cloneBoard = new Board(board, lastMove)
      val moveArr = cloneBoard.getPossibleMoves(opposition)

      for(m <- moveArr) {

        val childBoard = new Board(cloneBoard,m)
        children = children :+ new State(opposition, childBoard, m)

      }

    } catch {
      case e: IllegalArgumentException => System.err.println("Move not possible")
    }

  }


  /**
   * Write this State to a file called "output.txt", including its
   * children, their children, etc.. This method allows the State to
   * be viewed in a file even when it is too large to print to console.
   * Beep when printing is done.
   */
  def writeToFile() {
    println("write to file")
    var writer: PrintWriter = null
    try {
      writer = new PrintWriter("output.txt", "UTF-8")
      writer.println(this)
    } catch {
      case e@(_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
    } finally {
      writer.close();
    }
  }

  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }

  override def compareTo(o: Any): Int = ???

  /**
   * Return a string that contains a representation of this board indented
   * with string ind (expected to be a string of blank characters) followed
   * by a similar representation of all its children,
   * indented an additional ind characters. d is the depth of this state.
   */
  private def toStringHelper(d: Int, ind: String): String = {
    var str = ind + player + " to play\n"
    str = str + ind + "Value: " + value + "\n"
    str = str + board.toString(ind) + "\n"
    if (children != null && children.length > 0) {
      str = str + ind + "Children at depth " + (d + 1) + ":\n" + ind +
        "----------------\n"
      for (s <- children) {
        str = str + s.toStringHelper(d + 1, ind + "   ")
      }
    }
    str
  }
}

object State {

  val length0 = Array[State]()
}

